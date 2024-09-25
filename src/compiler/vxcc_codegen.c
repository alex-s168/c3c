#include "build/build.h"
#include "utils/common.h"
#include "utils/lib.h"
#include "vxcc_codegen_internal.h"
#include <stdio.h>

// we use fastalloc during compile module and then fastfreeall when module done

VxccVarDecl* vxcc_var(Decl* decl)
{
    assert(decl->decl_kind == DECL_VAR);

    if (decl->backend_ref)
    {
        return decl->backend_ref;
    }

    VxccCU* cu = decl->unit->vxcc;

    switch (decl->var.kind)
    {
        case VARDECL_MEMBER:
        case VARDECL_BITMEMBER:
        // compile time vars don't need vxcc var 
        case VARDECL_PARAM_CT:
        case VARDECL_PARAM_CT_TYPE:
        case VARDECL_LOCAL_CT:
        case VARDECL_LOCAL_CT_TYPE:
        // global constant
        case VARDECL_CONST:
        // macro params
        case VARDECL_PARAM_REF:
        case VARDECL_PARAM_EXPR:
            break;

        case VARDECL_LOCAL:
        case VARDECL_PARAM: {
            VxccVarDecl* vxcc = fastalloc(sizeof(VxccVarDecl));
            memset(vxcc, 0, sizeof(VxccVarDecl));
            vxcc->vxVar = cu->nextVarId ++;
            decl->backend_ref = vxcc;
            return vxcc;
        }

        case VARDECL_GLOBAL: {
            error_exit("globals not yet supported by vxcc backend");
        }

        case VARDECL_UNWRAPPED:
        case VARDECL_ERASE:
        case VARDECL_REWRAPPED: {
            error_exit("TODO: %s:%d", __FILE__, __LINE__);
        }
    }

    assert(false);
    return NULL;
}

vx_IrType* vxcc_type(Type* type)
{
    assert(type != NULL);

    if (type->backend_type) return type->backend_type;

    if (type->type_kind == TYPE_TYPEDEF) {
        vx_IrType* ty = vxcc_type(type->canonical);
        type->backend_type = ty; 
        return ty;
    }

    vx_IrType* res = fastalloc(sizeof(vx_IrType));
    assert(res != NULL);
    memset(res, 0, sizeof(*res));

    type->backend_type = res;

    res->debugName = type->name;

    switch (type->type_kind)
    {
        case TYPE_POISONED: 
            error_exit("TYPE_POISONED not implemented");

        case TYPE_VOID: {
            res->kind = VX_IR_TYPE_KIND_BASE;
            res->base.size = 0;
            res->base.align = 0;
            res->base.isfloat = false;
            res->base.sizeless = false;
            break;
        }

        case TYPE_BOOL: {
            res->kind = VX_IR_TYPE_KIND_BASE;
            res->base.size = 1;
            res->base.align = 1;
            res->base.sizeless = false;
            res->base.isfloat = false;
            break;
        }

        case TYPE_POINTER: {
            res->kind = VX_IR_TYPE_KIND_BASE;
            res->base.size = compiler.platform.width_pointer / 8;
            res->base.align = compiler.platform.align_pointer.align;
            res->base.sizeless = false;
            res->base.isfloat = false;
            break;
        }

        // uint 
        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64:
        case TYPE_U128:
        // int
        case TYPE_I8: 
        case TYPE_I16:
        case TYPE_I32:
        case TYPE_I64:
        case TYPE_I128: {
            res->kind = VX_IR_TYPE_KIND_BASE;
            res->base.size = type->builtin.bytesize;
            res->base.align = type->builtin.abi_alignment;
            res->base.sizeless = false;
            res->base.isfloat = false;
            break;
        }

        case TYPE_F16:
        case TYPE_F32:
        case TYPE_F64:
        case TYPE_F128: {
            res->kind = VX_IR_TYPE_KIND_BASE;
            res->base.size = type->builtin.bytesize;
            res->base.align = type->builtin.abi_alignment;
            res->base.sizeless = false;
            res->base.isfloat = true;
            break;
        }

        case TYPE_STRUCT: {
            StructDecl* decl = &type->decl->strukt;

            res->kind = VX_IR_TYPE_KIND_CIR_STRUCT;
            res->cir_struct.pack = false;
            res->cir_struct.align = decl->size;
            res->cir_struct.members_len = vec_size(decl->members);
            res->cir_struct.members = calloc(vec_size(decl->members), sizeof(vx_IrType*));
            for (size_t i = 0; i < vec_size(decl->members); i ++) {
                res->cir_struct.members[i] = vxcc_type(decl->members[i]->type);
            }
            break;
        }

        default: {
            error_exit("VXCC currently doesn't support %s type (typekind%i)", type->name, type->type_kind);
        }
    }

    printf("type %s has size of %zu\n", type->name, vx_IrType_size(res));

    return res;
}

static vx_IrBlock* vxcc_emit_function_body(VxccCU* cu, Decl* decl)
{
    assert(decl != NULL);
    FuncDecl* fn = &decl->func_decl;
    assert(fn != NULL);

    vx_IrBlock* block = fastalloc(sizeof(vx_IrBlock));
    assert(block != NULL);
    vx_IrBlock_init(block, NULL, NULL); // root block 
    vx_IrBlock_makeRoot(block, 0); // zero vars (for now) 
    block->name = decl->name;

    
    Type* retTy = typeget(fn->signature.rtype);
    if (retTy && retTy->type_kind == TYPE_VOID)
    {
        retTy = NULL;
    }
    
    // return value 
    if (retTy)
    {
        // create op that assigns "uninit" to the return var (which then maybe gets returned eventually)
        vx_IrOp* initOp = vx_IrBlock_addOpBuilding(block);
        vx_IrVar outVar = vx_IrBlock_newVar(block, initOp);
        assert(outVar == 0);
        vx_IrOp_init(initOp, VX_IR_OP_IMM, block);
        vx_IrOp_addOut(initOp, outVar, vxcc_type(retTy));
        vx_IrOp_addParam_s(initOp, VX_IR_NAME_VALUE, VX_IR_VALUE_UNINIT());

        vx_IrBlock_addOut(block,  outVar);
    }

    // parameters
    FOREACH(Decl *, param, fn->signature.params)
    {
        VxccVarDecl* vxcc = vxcc_var(param);
        Type* ty = typeget(param->var.type_info);
        vx_IrBlock_addIn(block, vxcc->vxVar, vxcc_type(ty));
    }

    Ast* body = astptrzero(fn->body);

    for (; body; body = astptrzero(body->next))
    {
        vxcc_emit_stmt(block, cu, body);
    }

    // no need for implicit return because we are already at tail and the return var is initialized with undefined

    vx_CIrBlock_fix(block);

    return block;
}

static void vxcc_gen_cu(Module* parent, CompilationUnit* cu, vx_CU* vx_cu)
{
    VxccCU* vxcu = fastalloc(sizeof(VxccCU));
    assert(vxcu);
    memset(vxcu, 0, sizeof(VxccCU));

    vxcu->nextVarId = 1; // see vxcc_codegen_internal.h for why this is 1

    cu->vxcc = vxcu;

	bool only_used = strip_unused();

    FOREACH(Decl *, decl, cu->functions)
    {
        if (decl->func_decl.attr_test && !compiler.build.testing) continue;
        if (decl->func_decl.attr_benchmark && !compiler.build.benchmarking) continue;
        if (only_used && !decl->is_live) continue;
        if (decl->func_decl.body)
        {
            vx_IrBlock* block = vxcc_emit_function_body(vxcu, decl);
            vx_CU_addIrBlock(vx_cu, block, decl->is_export);
        }
    }

    FOREACH(Decl *, func, cu->lambdas)
    {
        if (only_used && !func->is_live) continue;
        vx_IrBlock* blk = vxcc_emit_function_body(vxcu, func);
        vx_CU_addIrBlock(vx_cu, blk, /*export=*/ false);
    }

    if (compiler.build.type != TARGET_TYPE_TEST && compiler.build.type != TARGET_TYPE_BENCHMARK && cu->main_function && cu->main_function->is_synthetic)
    {
        vx_IrBlock* blk = vxcc_emit_function_body(vxcu, cu->main_function);
        vx_CU_addIrBlock(vx_cu, blk, /*export=*/ true);
    }

    FOREACH(Decl *, decl, cu->methods)
    {
        if (only_used && !decl->is_live) continue;
        if (!decl->func_decl.body) continue;
        vx_IrBlock* block = vxcc_emit_function_body(vxcu, decl);
        vx_CU_addIrBlock(vx_cu, block, decl->is_export);
    }
}

static void vxcc_set_opt_flags(void)
{
    // TODO:opt flags  

    vx_g_optconfig.if_eval = true;
    vx_g_optconfig.loop_simplify = true;
    vx_g_optconfig.consteval_iterations = 6;
    vx_g_optconfig.max_total_cmov_inline_cost = 4;
}

void **vxcc_gen(Module** modules, unsigned module_count)
{
    vxcc_set_opt_flags();

	if (!module_count) return NULL;
    if (compiler.build.emit_object_files)
	{
        if (compiler.build.benchmarking)
		{
            error_exit("vxcc backend currly does not support benchmarks");
		}
		if (compiler.build.testing)
		{
            error_exit("vxcc backend currly does not support tests");
		}

        vx_CU** cus = NULL;

        for (size_t m = 0; m < module_count; m ++)
        {
            Module* module = modules[m];
            FOREACH(CompilationUnit *, unit, module->units)
            {
                vx_CU* cu = fastalloc(sizeof(vx_CU));
                memset(cu, 0, sizeof(vx_CU));
                vec_add(cus, cu);
                vxcc_gen_cu(module, unit, cu);
            }
        }

        // TODO 
        // llvm_emit_dynamic_functions(gen_context, gen_context->dynamic_functions);
        // llvm_emit_constructors_and_destructors(gen_context);

		return (void**) cus;
	}
    else
    {
        error_exit("vxcc backend currently only supports generating object files");
    }
}

static void vxcc_set_target(vx_Target* dest) {
    switch (compiler.platform.arch)
    {
        case ARCH_TYPE_X86_64:
            dest->arch = VX_TARGET_X86_64;
            break;

        default:
            error_exit("architecture not supported by vxcc");
    }
}

const char *vxcc_codegen(void *context)
{
	vx_CU* cu = context;
    vxcc_set_target(&cu->target);

    FILE* optionalOptimizedSsaIr = stdout; // TODO: remove
    FILE* optionalOptimizedLlIr = stdout;  //       ^^^^
    FILE* optionalAsm = stdout;
    vx_BinFormat optionalBinFormat = 0; FILE* optionalBinOut = NULL;

    int res = vx_CU_compile(cu,
                optionalOptimizedSsaIr,
                optionalOptimizedLlIr,
                optionalAsm,
                optionalBinFormat, optionalBinOut);

    if (optionalOptimizedSsaIr && optionalOptimizedSsaIr != stdout)
    {
        fclose(optionalOptimizedSsaIr);
    }

    if (optionalOptimizedLlIr && optionalOptimizedLlIr != stdout)
    {
        fclose(optionalOptimizedLlIr);
    }

    if (optionalAsm && optionalAsm != stdout)
    {
        fclose(optionalAsm);
    }

    if (optionalBinOut && optionalBinOut != stdout)
    {
        fclose(optionalBinOut);
    }

    if (res)
    {
        return NULL;
    }
    return "fake";
}

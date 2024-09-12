#include "utils/common.h"
#include "vxcc_codegen_internal.h"

static vx_IrType* vxcc_conv_type(Type* type)
{
    if (type->backend_type) return type->backend_type;

    vx_IrType* res = fastalloc(sizeof(vx_IrType));
    memset(res, 0, sizeof(*res));

    type->backend_type = res;

    res->debugName = type->name;

    switch (type->type_kind)
    {
        case TYPE_POISONED: 
            error_exit("TYPE_POISONED not implemented");

        case TYPE_VOID: {
            res->kind = VX_IR_TYPE_KIND_BASE;
            res->base.pad = 0;
            res->base.size = 0;
            res->base.align = 0;
            res->base.isfloat = false;
            res->base.sizeless = false;
            break;
        }

        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64:
        case TYPE_U128:
        case TYPE_I8: 
        case TYPE_I16:
        case TYPE_I32:
        case TYPE_I64:
        case TYPE_I128: {
            res->kind = VX_IR_TYPE_KIND_BASE;
            res->base.pad = 0;
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
            res->base.pad = 0;
            res->base.size = type->builtin.bytesize;
            res->base.align = type->builtin.abi_alignment;
            res->base.sizeless = false;
            res->base.isfloat = true;
            break;
        }

        default: {
            error_exit("VXCC currently doesn't support %s type", type->name);
        }
    }

    return res;
}

static void vxcc_gen_function_decl(Decl* method) 
{
    // TODO 
}

static void vxcc_emit_type_decl(Decl* type_decl)
{
    // TODO 
}

static void vxcc_emit_function_decl(Decl* method) 
{
    // TODO 
}

static void vxcc_get_ref(Decl* decl)
{
    // TODO
}

static void vxcc_emit_global_variable_init(Decl* decl)
{
    // TODO
}

static void vxcc_emit_function_body(Decl* decl)
{
    FuncDecl* fn = &decl->func_decl;
    // TODO
}

static void vxcc_gen_cu_pre(Module* parent, CompilationUnit* cu)
{
	bool only_used = strip_unused();

    FOREACH(Decl *, method, cu->methods)
    {
        if (only_used && !method->is_live) continue;
        vxcc_gen_function_decl(method);
    }

    FOREACH(Decl *, type_decl, cu->types)
    {
        if (only_used && !type_decl->is_live) continue;
        vxcc_emit_type_decl(type_decl);
    }

    FOREACH(Decl *, enum_decl, cu->enums)
    {
        if (only_used && !enum_decl->is_live) continue;
        vxcc_emit_type_decl(enum_decl);
    }

    FOREACH(Decl *, func, cu->functions)
    {
        if (only_used && !func->is_live) continue;
        if (func->func_decl.attr_test)
        {
            if (!compiler.build.testing) continue;
            // TODO
        }
        if (func->func_decl.attr_benchmark)
        {
            if (!compiler.build.benchmarking) continue;
            // TODO 
        }
        vxcc_emit_function_decl(func);
    }

    FOREACH(Decl *, func, cu->lambdas)
    {
        if (only_used && !func->is_live) continue;
        vxcc_emit_function_decl(func);
    }

    if (compiler.build.type != TARGET_TYPE_TEST && compiler.build.type != TARGET_TYPE_BENCHMARK && cu->main_function && cu->main_function->is_synthetic)
    {
        vxcc_emit_function_decl(cu->main_function);
    }
}

static void vxcc_gen_cu_post(Module* parent, CompilationUnit* cu )
{
	bool only_used = strip_unused();

    FOREACH(Decl *, var, cu->vars)
    {
        if (only_used && !var->is_live) continue;
        vxcc_get_ref(var);
    }

    FOREACH(Decl *, var, cu->vars)
    {
        if (only_used && !var->is_live) continue;
        vxcc_emit_global_variable_init(var);
    }

    FOREACH(Decl *, decl, cu->functions)
    {
        if (decl->func_decl.attr_test && !compiler.build.testing) continue;
        if (decl->func_decl.attr_benchmark && !compiler.build.benchmarking) continue;
        if (only_used && !decl->is_live) continue;
        if (decl->func_decl.body)
        {
            vxcc_emit_function_body(decl);
        }
    }

    FOREACH(Decl *, func, cu->lambdas)
    {
        if (only_used && !func->is_live) continue;
        vxcc_emit_function_body(func);
    }

    if (compiler.build.type != TARGET_TYPE_TEST && compiler.build.type != TARGET_TYPE_BENCHMARK && cu->main_function && cu->main_function->is_synthetic)
    {
        vxcc_emit_function_body(cu->main_function);
    }

    FOREACH(Decl *, decl, cu->methods)
    {
        if (only_used && !decl->is_live) continue;
        if (!decl->func_decl.body) continue;
        vxcc_emit_function_body(decl);
    }
}

void **vxcc_gen(Module** modules, unsigned module_count)
{
	if (!module_count) return NULL;
    if (compiler.build.single_module == SINGLE_MODULE_ON)
	{
		Module* module = modules[0];

        if (compiler.build.benchmarking)
		{
            error_exit("vxcc backend currly does not support benchmarks");
		}
		if (compiler.build.testing)
		{
            error_exit("vxcc backend currly does not support tests");
		}

        FOREACH(CompilationUnit *, unit, module->units)
        {
            vxcc_gen_cu_pre(module, unit);
        }

        FOREACH(CompilationUnit *, unit, module->units)
        {
            vxcc_gen_cu_post(module, unit);
        }

        // TODO 
        // llvm_emit_dynamic_functions(gen_context, gen_context->dynamic_functions);
        // llvm_emit_constructors_and_destructors(gen_context);

		return NULL;
	}
    else
    {
        error_exit("vxcc backend currently only supports generating object files");
    }
	
}

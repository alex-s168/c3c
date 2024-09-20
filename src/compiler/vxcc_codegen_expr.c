#include "utils/common.h"
#include "vxcc_codegen_internal.h"

static void vxcc_emit_mutateExpr(vx_IrBlock* dest_block, VxccCU* cu, Expr* expr, vx_IrValue newVal, Type* newValType);

static vx_IrVar vxcc_emit_binary(vx_IrBlock* dest_block, VxccCU* cu, Expr* expr, vx_IrType* outTy)
{
    ExprBinary* bin = &expr->binary_expr;

    vx_OptIrVar left = vxcc_emit_expr(dest_block, cu, exprptr(bin->left));
    vx_OptIrVar right = vxcc_emit_expr(dest_block, cu, exprptr(bin->right));

    if (!left.present || !right.present) 
    {
        error_exit("codegen error: right / left side of binary expr does not return value?");
    }

    vx_IrOpType ty;
    switch (bin->operator)
    {
        case BINARYOP_CT_AND:
        case BINARYOP_CT_OR:
        case BINARYOP_CT_CONCAT:
        case BINARYOP_BIT_XOR_ASSIGN:
        case BINARYOP_BIT_XOR:
        case BINARYOP_ELSE:
        case BINARYOP_ERROR: {
            error_exit("binary op %i not supported by VXCC backend", bin->operator);
            break;
        }

        case BINARYOP_MULT_ASSIGN:
        case BINARYOP_MULT: ty = VX_IR_OP_MUL; break;
        case BINARYOP_SUB_ASSIGN:
        case BINARYOP_SUB: ty = VX_IR_OP_SUB; break;
        case BINARYOP_ADD_ASSIGN:
        case BINARYOP_ADD: ty = VX_IR_OP_ADD; break;
        case BINARYOP_DIV_ASSIGN:
        case BINARYOP_DIV: ty = type_is_signed(expr->type) ? VX_IR_OP_SDIV : VX_IR_OP_UDIV; break;
        case BINARYOP_MOD_ASSIGN:
        case BINARYOP_MOD: ty = VX_IR_OP_MOD; break;
        case BINARYOP_SHR_ASSIGN:
        case BINARYOP_SHR: ty = type_is_signed(expr->type) ? VX_IR_OP_ASHR : VX_IR_OP_SHR; break;
        case BINARYOP_SHL_ASSIGN:
        case BINARYOP_SHL: ty = VX_IR_OP_SHL; break;
        case BINARYOP_BIT_OR: ty = VX_IR_OP_BITIWSE_OR; break;
        case BINARYOP_BIT_AND_ASSIGN:
        case BINARYOP_BIT_AND: ty = VX_IR_OP_BITWISE_AND; break;
        case BINARYOP_AND: ty = VX_IR_OP_AND; break;
        case BINARYOP_BIT_OR_ASSIGN:
        case BINARYOP_OR: ty = VX_IR_OP_OR; break;

        case BINARYOP_GT:
        case BINARYOP_GE:
        case BINARYOP_LT:
        case BINARYOP_LE: {
            bool lhs_signed = type_is_signed(exprptr(bin->left)->type);
            bool rhs_signed = type_is_signed(exprptr(bin->right)->type);

            if (lhs_signed == rhs_signed) {
                vx_IrOpType ty;

                if (lhs_signed) {
                    switch (bin->operator)
                    {
                        case BINARYOP_GT: ty = VX_IR_OP_SGT; break;
                        case BINARYOP_GE: ty = VX_IR_OP_SGTE; break;
                        case BINARYOP_LT: ty = VX_IR_OP_SLT; break;
                        case BINARYOP_LE: ty = VX_IR_OP_SLTE; break;
                        default: UNREACHABLE;
                    }
                } else {
                    switch (bin->operator)
                    {
                        case BINARYOP_GT: ty = VX_IR_OP_UGT; break;
                        case BINARYOP_GE: ty = VX_IR_OP_UGTE; break;
                        case BINARYOP_LT: ty = VX_IR_OP_ULT; break;
                        case BINARYOP_LE: ty = VX_IR_OP_ULTE; break;
                        default: UNREACHABLE;
                    }
                }

                vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
                vx_IrOp_init(op, ty, dest_block);
                vx_IrVar outVar = cu->nextVarId ++;
                vx_IrOp_addOut(op, outVar, vxcc_type(expr->type));
                vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_A, VX_IR_VALUE_VAR(left.var));
                vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_B, VX_IR_VALUE_VAR(right.var));
                return outVar;
            }

            vx_IrOpType ty;
            if (rhs_signed) { // swap
                rhs_signed = false; lhs_signed = true;
                vx_OptIrVar temp = left; left = right; right = temp;

                switch (bin->operator)
                {
                    case BINARYOP_GT: ty = VX_IR_OP_ULT; break;
                    case BINARYOP_GE: ty = VX_IR_OP_ULTE; break;
                    case BINARYOP_LT: ty = VX_IR_OP_UGT; break;
                    case BINARYOP_LE: ty = VX_IR_OP_UGTE; break;
                    default: UNREACHABLE;
                }
            } else {
                switch (bin->operator)
                {
                    case BINARYOP_GT: ty = VX_IR_OP_UGT; break;
                    case BINARYOP_GE: ty = VX_IR_OP_UGTE; break;
                    case BINARYOP_LT: ty = VX_IR_OP_ULT; break;
                    case BINARYOP_LE: ty = VX_IR_OP_ULTE; break;
                    default: UNREACHABLE;
                }
            }

            // lhs is signed 

            vx_IrVar lgte0 = cu->nextVarId ++;
            { // left >= 0
                vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
                vx_IrOp_init(op, VX_IR_OP_SGTE, dest_block);
                vx_IrOp_addOut(op, lgte0, vxcc_type(expr->type));
                vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_A, VX_IR_VALUE_VAR(left.var));
                vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_B, VX_IR_VALUE_IMM_INT(0));
            }

            vx_IrVar cmp = cu->nextVarId ++;
            { // left CMP right
                vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
                vx_IrOp_init(op, ty, dest_block);
                vx_IrOp_addOut(op, cmp, vxcc_type(expr->type));
                vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_A, VX_IR_VALUE_VAR(left.var));
                vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_B, VX_IR_VALUE_VAR(right.var));
            }

            vx_IrVar out = cu->nextVarId ++;
            { // lgte0 && cmp
                vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
                vx_IrOp_init(op, VX_IR_OP_AND, dest_block);
                vx_IrOp_addOut(op, out, vxcc_type(expr->type));
                vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_A, VX_IR_VALUE_VAR(lgte0));
                vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_B, VX_IR_VALUE_VAR(cmp));
            }

            return out;
        }

        case BINARYOP_NE: ty = VX_IR_OP_NEQ; break;
        case BINARYOP_EQ: ty = VX_IR_OP_EQ; break;

        case BINARYOP_ASSIGN: { // return (left = right)
            vxcc_emit_mutateExpr(dest_block, cu, exprptr(bin->left), VX_IR_VALUE_VAR(right.var), expr->type);
            return right.var;
        }
    }

    bool isAssign = bin->operator >= BINARYOP_ASSIGN; // return (left += right) 

    vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
    vx_IrOp_init(op, ty, dest_block);

    assert(outTy != NULL);
    vx_IrVar out = cu->nextVarId ++;
    vx_IrOp_addOut(op, out, outTy);

    vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_A, VX_IR_VALUE_VAR(right.var));
    vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_B, VX_IR_VALUE_VAR(left.var));

    if (isAssign) {
        vxcc_emit_mutateExpr(dest_block, cu, exprptr(bin->left), VX_IR_VALUE_VAR(out), expr->type);
    }

    return out;
}

static vx_IrVar vxcc_emit_subscriptExprAddr(vx_IrBlock* dest_block, VxccCU* cu, Expr* expr)
{
    ExprSubscript* sc = &expr->subscript_expr;

    // ptr
    Expr* arr_expr = exprptr(sc->expr);
    vx_OptIrVar array = vxcc_emit_expr(dest_block, cu, arr_expr);
    assert(array.present);
    
    vx_OptIrVar index = vxcc_emit_expr(dest_block, cu, exprptr(sc->index.expr));
    assert(index.present);

    if (sc->index.start_from_end)
    {
        error_exit("start from end indexing not yet supported by VXCC backend");
    }

    vx_IrVar temp = cu->nextVarId ++;
    vx_IrVar ptr = cu->nextVarId ++;

    vx_IrOp* mul = vx_IrBlock_addOpBuilding(dest_block);
    vx_IrOp_init(mul, VX_IR_OP_MUL, dest_block);
    vx_IrOp_addParam_s(mul, VX_IR_NAME_OPERAND_A, VX_IR_VALUE_VAR(index.var));
    vx_IrOp_addParam_s(mul, VX_IR_NAME_OPERAND_B, VX_IR_VALUE_IMM_INT(type_size(expr->type))); // TODO: verify
    vx_IrOp_addOut(mul, temp, vxcc_type(arr_expr->type));

    vx_IrOp* add = vx_IrBlock_addOpBuilding(dest_block);
    vx_IrOp_init(add, VX_IR_OP_ADD, dest_block);
    vx_IrOp_addParam_s(add, VX_IR_NAME_OPERAND_A, (vx_IrValue) {.type = VX_IR_VAL_VAR, .var = array.var });
    vx_IrOp_addParam_s(add, VX_IR_NAME_OPERAND_B, (vx_IrValue) {.type = VX_IR_VAL_VAR, .var = temp });
    vx_IrOp_addOut(add, ptr, vxcc_type(arr_expr->type));

    return ptr;
}

static CastKind cast_invert(CastKind old)
{
    switch (old)
    {
        // int size casts 
        case CAST_BOOLBOOL: return CAST_BOOLBOOL;
        case CAST_BOOLINT: return CAST_INTBOOL;
        case CAST_INTINT: return CAST_INTINT;
        case CAST_INTBOOL: return CAST_BOOLINT;
        case CAST_PTRBOOL: return CAST_INTPTR;
        case CAST_PTRINT: return CAST_INTPTR;
        case CAST_INTPTR: return CAST_PTRINT;
        case CAST_PTRPTR: return CAST_PTRPTR;
        case CAST_BOOLFP: return CAST_FPBOOL;
        case CAST_INTFP: return CAST_FPINT;
        case CAST_FPBOOL: return CAST_BOOLFP;
        case CAST_FPINT: return CAST_INTFP;
        case CAST_FPFP: return CAST_FPFP;
        default: {
            error_exit("cast kind %i currenlty not supported by VXCC backend", old);
            break;
        }
    }
}

static void vxcc_emit_cast(vx_IrBlock* dest_block, VxccCU* cu, vx_IrVar dest, Type* destType, vx_IrVar src, Type* srcType, CastKind kind)
{
    switch (kind)
    {
        // int size casts 
        case CAST_BOOLBOOL:
        case CAST_BOOLINT:
        case CAST_INTINT:
        case CAST_INTBOOL:
        case CAST_PTRBOOL:
        case CAST_PTRINT:
        case CAST_INTPTR:
        case CAST_PTRPTR: {
            vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
            
            size_t old_si = type_size(srcType);
            size_t new_si = type_size(destType);
            
            vx_IrOpType op_kind;
            if (old_si >= new_si)
                op_kind = VX_IR_OP_BITCAST;
            else if (type_is_signed(destType)) 
                op_kind = VX_IR_OP_SIGNEXT;
            else 
                op_kind = VX_IR_OP_ZEROEXT;
            
            vx_IrOp_init(op, op_kind, dest_block);
            vx_IrOp_addParam_s(op, VX_IR_NAME_VALUE, VX_IR_VALUE_VAR(src));
            vx_IrOp_addOut(op, dest, vxcc_type(destType));
            break;
        }

        // int -> float casts
        case CAST_BOOLFP:
        case CAST_INTFP: {
            vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
            vx_IrOp_init(op, VX_IR_OP_TOFLT, dest_block);
            vx_IrOp_addParam_s(op, VX_IR_NAME_VALUE, VX_IR_VALUE_VAR(src));
            vx_IrVar out = cu->nextVarId ++;
            vx_IrOp_addOut(op, out, vxcc_type(destType));
            break;
        }

        // float -> int casts 
        case CAST_FPBOOL:
        case CAST_FPINT: {
            vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
            vx_IrOp_init(op, VX_IR_OP_FROMFLT, dest_block);
            vx_IrOp_addParam_s(op, VX_IR_NAME_VALUE, VX_IR_VALUE_VAR(src));
            vx_IrVar out = cu->nextVarId ++;
            vx_IrOp_addOut(op, out, vxcc_type(destType));
            break;
        }

        // noop
        case CAST_VOID: {
            break;
        }

        // float size casts
        case CAST_FPFP: {
            error_exit("floating point size casts are currenlty not supported by VXCC backend");
            break;
        }

        // other weird things
        case CAST_ANYPTR:
        case CAST_ANYBOOL:
        case CAST_INTENUM: 
        case CAST_APTSA:
        case CAST_ARRVEC:
        case CAST_BOOLVECINT:
        case CAST_BSINTARR:
        case CAST_INTARRBS:
        case CAST_EREU:
        case CAST_ERINT:
        case CAST_ERPTR:
        case CAST_ERROR:
        case CAST_EUBOOL:
        case CAST_EUER:
        case CAST_IDPTR:
        case CAST_IDBOOL:
        case CAST_IDINT:
        case CAST_PTRANY:
        case CAST_SLBOOL:
        case CAST_SAPTR:
        case CAST_SLSL:
        case CAST_SLARR:
        case CAST_STRPTR:
        case CAST_STINLINE:
        case CAST_VECARR:
        case CAST_INTERR:
        case CAST_EXPVEC: {
            error_exit("cast kind %i currenlty not supported by VXCC backend", kind);
            break;
        }
    }
}

static void vxcc_emit_mutateExpr(vx_IrBlock* dest_block, VxccCU* cu, Expr* expr, vx_IrValue newVal, Type* newValType)
{
    switch (expr->expr_kind)
    {
        case EXPR_IDENTIFIER: {
            Decl* ident = expr->identifier_expr.decl;
            switch (ident->decl_kind)
            {
                case DECL_VAR: {
                    vx_IrVar var = vxcc_var(ident)->vxVar;

                    vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
                    vx_IrOp_init(op, VX_IR_OP_IMM, dest_block);
                    vx_IrOp_addOut(op, var, vxcc_type(expr->type));
                    vx_IrOp_addParam_s(op, VX_IR_NAME_VALUE, newVal);
                    break;
                }

                default:
                    error_exit("unsupported (vxcc) expr_decl decl kind");
            }

            break;
        }

        case EXPR_SUBSCRIPT: {
            vx_IrVar ptr = vxcc_emit_subscriptExprAddr(dest_block, cu, expr);

            vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
            vx_IrOp_init(op, VX_IR_OP_STORE, dest_block);
            vx_IrOp_addParam_s(op, VX_IR_NAME_ADDR, VX_IR_VALUE_VAR(ptr));
            vx_IrOp_addParam_s(op, VX_IR_NAME_VALUE, newVal);
            break;
        }

        case EXPR_CAST: {
            Expr* inner = exprptr(expr->cast_expr.expr);
            CastKind un_cast = cast_invert(expr->cast_expr.kind);

            vx_IrVar newValAsVar = cu->nextVarId ++;
            {
                vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
                vx_IrOp_init(op, VX_IR_OP_IMM, dest_block);
                vx_IrOp_addOut(op, newValAsVar, vxcc_type(newValType));
                vx_IrOp_addParam_s(op, VX_IR_NAME_VALUE, newVal);
            }

            vx_IrVar temp = cu->nextVarId ++;
            vxcc_emit_cast(dest_block, cu,
                           temp, inner->type,
                           newValAsVar, newValType,
                           un_cast);

            vxcc_emit_mutateExpr(dest_block, cu, inner, VX_IR_VALUE_VAR(temp), inner->type);

            break;
        }

        case EXPR_DECL: {
            assert(expr->decl_expr->decl_kind == DECL_VAR);
            VarDecl* vd = &expr->decl_expr->var;
            assert(vd->kind == VARDECL_LOCAL);
            vx_IrVar var = vxcc_var(expr->decl_expr)->vxVar;

            vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
            vx_IrOp_init(op, VX_IR_OP_IMM, dest_block);
            vx_IrOp_addOut(op, var, vxcc_type(expr->type));
            vx_IrOp_addParam_s(op, VX_IR_NAME_VALUE, newVal);
            break;
        }

        default: {
            error_exit("mutating expression kind %i is currenlty not supported by VXCC backend", expr->expr_kind);
            break;
        }
    }
}

static vx_OptIrVar vxcc_emit_constexpr(vx_IrBlock* dest_block, VxccCU* cu, vx_IrType* outTy, vx_IrValue value)
{
    vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
    vx_IrOp_init(op, VX_IR_OP_IMM, dest_block);
    vx_IrVar outVar = cu->nextVarId ++;
    vx_IrOp_addOut(op, outVar, outTy);
    vx_IrOp_addParam_s(op, VX_IR_NAME_VALUE, value);
    return VX_IRVAR_OPT_SOME(outVar);
}

vx_OptIrVar vxcc_emit_expr(vx_IrBlock* dest_block, VxccCU* cu, Expr* expr)
{
    assert(expr);
    vx_IrType* outTy = expr->type ? vxcc_type(expr->type) : NULL;
    switch (expr->expr_kind) 
    {
        case EXPR_BINARY: {
            return VX_IRVAR_OPT_SOME(vxcc_emit_binary(dest_block, cu, expr, outTy));
        }

        case EXPR_IDENTIFIER: {
            Decl* ident = expr->identifier_expr.decl;
            switch (ident->decl_kind)
            {
                case DECL_VAR:
                    return VX_IRVAR_OPT_SOME(vxcc_var(ident)->vxVar);

                default:
                    error_exit("unsupported (vxcc) expr_decl decl kind");
            }

            break;
        }

        case EXPR_DECL: {
            assert(expr->decl_expr->decl_kind == DECL_VAR);
            VarDecl* vd = &expr->decl_expr->var;
            assert(vd->kind == VARDECL_LOCAL);
            Expr* initexp = vd->init_expr;
            if (initexp) {
                vx_OptIrVar init = vxcc_emit_expr(dest_block, cu, initexp);
                assert(init.present);

                vx_IrVar dest = vxcc_var(expr->decl_expr)->vxVar;

                vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
                vx_IrOp_init(op, VX_IR_OP_IMM, dest_block);
                vx_IrOp_addParam_s(op, VX_IR_NAME_VALUE, VX_IR_VALUE_VAR(init.var));
                vx_IrOp_addOut(op, dest, vxcc_type(expr->decl_expr->type));

                return init;
            } else {
                return VX_IRVAR_OPT_NONE;
            }
        }

        case EXPR_CAST: {
            Expr* to_cast_expr = exprptr(expr->cast_expr.expr);
            vx_OptIrVar to_cast = vxcc_emit_expr(dest_block, cu, to_cast_expr);
            assert(to_cast.present);
            vx_IrVar var = cu->nextVarId ++;
            vxcc_emit_cast(dest_block, cu,
                           var, expr->type, 
                           to_cast.var, to_cast_expr->type,
                           expr->cast_expr.kind);
            return VX_IRVAR_OPT_SOME(var);
        }

        case EXPR_COND: {
            size_t s = vec_size(expr->cond_expr);
            size_t i = 0;
            // emit all except last expr and ignore ret val
            for (; i + 1 < s; i ++)
            {
                (void) vxcc_emit_expr(dest_block, cu, expr->cond_expr[i]);
            }

            return vxcc_emit_expr(dest_block, cu, expr->cond_expr[i]); // last expr 
        }

        case EXPR_EXPRESSION_LIST: {
            size_t s = vec_size(expr->expression_list);
            size_t i = 0;
            // emit all except last expr and ignore ret val
            for (; i + 1 < s; i ++)
            {
                (void) vxcc_emit_expr(dest_block, cu, expr->expression_list[i]);
            }

            return vxcc_emit_expr(dest_block, cu, expr->expression_list[i]); // last expr       
        }

        case EXPR_CONST: {
            switch (expr->const_expr.const_kind)
            {
                case CONST_FLOAT: {
                    return vxcc_emit_constexpr(dest_block, cu, outTy,
                                               (vx_IrValue) {.type = VX_IR_VAL_IMM_FLT,.imm_flt = expr->const_expr.fxx.f});
                }

                case CONST_INTEGER: {
                    return vxcc_emit_constexpr(dest_block, cu, outTy,
                                               (vx_IrValue) {.type = VX_IR_VAL_IMM_INT,.imm_int = expr->const_expr.ixx.i.low});
                }

                case CONST_BOOL: {
                    return vxcc_emit_constexpr(dest_block, cu, outTy,
                                               (vx_IrValue) {.type = VX_IR_VAL_IMM_INT,.imm_int = expr->const_expr.b});
                }

                case CONST_ENUM: {
                    return vxcc_emit_constexpr(dest_block, cu, outTy,
                                               (vx_IrValue) {.type = VX_IR_VAL_IMM_INT,.imm_int = expr->const_expr.enum_err_val->enum_constant.ordinal});
                }

                case CONST_ERR:
                case CONST_BYTES:
                case CONST_STRING:
                case CONST_POINTER:
                case CONST_TYPEID:
                case CONST_INITIALIZER:
                case CONST_UNTYPED_LIST:
                case CONST_MEMBER:
                    error_exit("constexprr kind %i currenlty not supported by VXCC backend", expr->const_expr.const_kind);
            }
            break;
        }

        case EXPR_SUBSCRIPT: {
            vx_IrVar ptr = vxcc_emit_subscriptExprAddr(dest_block, cu, expr);
            vx_IrVar out = cu->nextVarId ++;

            vx_IrOp* lod = vx_IrBlock_addOpBuilding(dest_block);
            vx_IrOp_init(lod, VX_IR_OP_LOAD, dest_block);
            vx_IrOp_addParam_s(lod, VX_IR_NAME_ADDR, VX_IR_VALUE_VAR(ptr));
            vx_IrOp_addOut(lod, out, outTy);

            return VX_IRVAR_OPT_SOME(out);
        }

        case EXPR_POST_UNARY: {
            Expr* inner = expr->unary_expr.expr;
            bool dec = expr->unary_expr.operator == UNARYOP_DEC;
            bool noWrap = expr->unary_expr.no_wrap;

            vx_OptIrVar old = vxcc_emit_expr(dest_block, cu, inner);
            assert(old.present);

            vx_IrOpType type = dec ? VX_IR_OP_SUB : VX_IR_OP_ADD;

            vx_IrVar changed = cu->nextVarId ++;
            vx_IrOp* op = vx_IrBlock_addOpBuilding(dest_block);
            vx_IrOp_init(op, type, dest_block);
            vx_IrOp_addOut(op, changed, vxcc_type(expr->type));
            vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_A, VX_IR_VALUE_VAR(old.var));
            vx_IrOp_addParam_s(op, VX_IR_NAME_OPERAND_B, VX_IR_VALUE_IMM_INT(1));

            vxcc_emit_mutateExpr(dest_block, cu, inner, VX_IR_VALUE_VAR(changed), expr->type);

            break;
        }

        default: {
            error_exit("expression type %i currenlty not supported by VXCC backend", expr->expr_kind);
            break;
        }
    }

    return VX_IRVAR_OPT_NONE;
}

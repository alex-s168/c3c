#include "vxcc_codegen_internal.h"

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
        case BINARYOP_SHR: {
            if (type_is_signed(expr->type))
            {
                error_exit("signed shift right (ASHR) not yet supported by VXCC backend");
            }
            ty = VX_IR_OP_SHR;
            break; 
        }
        case BINARYOP_SHL_ASSIGN:
        case BINARYOP_SHL: ty = VX_IR_OP_SHL; break;
        case BINARYOP_BIT_OR: ty = VX_IR_OP_BITIWSE_OR; break;
        case BINARYOP_BIT_AND_ASSIGN:
        case BINARYOP_BIT_AND: ty = VX_IR_OP_BITWISE_AND; break;
        case BINARYOP_AND: ty = VX_IR_OP_AND; break;
        case BINARYOP_BIT_OR_ASSIGN:
        case BINARYOP_OR: ty = VX_IR_OP_OR; break;
        case BINARYOP_GT: ty = type_is_signed(expr->type) ? VX_IR_OP_SGT : VX_IR_OP_UGT; break;
        case BINARYOP_GE: ty = type_is_signed(expr->type) ? VX_IR_OP_SGTE : VX_IR_OP_UGTE; break;
        case BINARYOP_LT: ty = type_is_signed(expr->type) ? VX_IR_OP_SLT : VX_IR_OP_ULT; break;
        case BINARYOP_LE: ty = type_is_signed(expr->type) ? VX_IR_OP_SLTE : VX_IR_OP_ULTE; break;
        case BINARYOP_NE: ty = VX_IR_OP_NEQ; break;
        case BINARYOP_EQ: ty = VX_IR_OP_EQ; break;

        case BINARYOP_ASSIGN: { // return (left = right)
            vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
            vx_IrOp_init(op, VX_IR_OP_IMM, dest_block);
            vx_IrOp_add_out(op, left.var, vxcc_type(exprptr(bin->left)->type));
            vx_IrOp_add_param_s(op, VX_IR_NAME_VALUE, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = right.var });
            return right.var;
        }
    }

    bool isAssign = bin->operator >= BINARYOP_ASSIGN; // return (left += right) 

    vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
    vx_IrOp_init(op, ty, dest_block);

    assert(outTy != NULL);
    vx_IrVar out = isAssign ? left.var : cu->nextVarId ++;
    vx_IrOp_add_out(op, out, outTy);

    vx_IrOp_add_param_s(op, VX_IR_NAME_OPERAND_A, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = right.var });
    vx_IrOp_add_param_s(op, VX_IR_NAME_OPERAND_B, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = left.var });

    return out;
}

static vx_OptIrVar vxcc_emit_constexpr(vx_IrBlock* dest_block, VxccCU* cu, vx_IrType* outTy, vx_IrValue value)
{
    vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
    vx_IrOp_init(op, VX_IR_OP_IMM, dest_block);
    vx_IrVar outVar = cu->nextVarId ++;
    vx_IrOp_add_out(op, outVar, outTy);
    vx_IrOp_add_param_s(op, VX_IR_NAME_VALUE, value);
    return VX_IRVAR_OPT_SOME(outVar);
}

vx_OptIrVar vxcc_emit_expr(vx_IrBlock* dest_block, VxccCU* cu, Expr* expr)
{
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

        case EXPR_COND: {
            assert(vec_size(expr->cond_expr) == 1);
            Expr* c0 = expr->cond_expr[0];
            return vxcc_emit_expr(dest_block, cu, c0);
            break; 
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
            vx_IrVar out = cu->nextVarId ++;

            vx_IrOp* mul = vx_IrBlock_add_op_building(dest_block);
            vx_IrOp_init(mul, VX_IR_OP_MUL, dest_block);
            vx_IrOp_add_param_s(mul, VX_IR_NAME_OPERAND_A, (vx_IrValue) {.type = VX_IR_VAL_VAR, .var = index.var });
            vx_IrOp_add_param_s(mul, VX_IR_NAME_OPERAND_B, (vx_IrValue) {.type = VX_IR_VAL_IMM_INT, .imm_int = type_size(expr->type) }); // TODO: verify
            vx_IrOp_add_out(mul, temp, vxcc_type(arr_expr->type));

            vx_IrOp* add = vx_IrBlock_add_op_building(dest_block);
            vx_IrOp_init(add, VX_IR_OP_ADD, dest_block);
            vx_IrOp_add_param_s(add, VX_IR_NAME_OPERAND_A, (vx_IrValue) {.type = VX_IR_VAL_VAR, .var = array.var });
            vx_IrOp_add_param_s(add, VX_IR_NAME_OPERAND_B, (vx_IrValue) {.type = VX_IR_VAL_VAR, .var = temp });
            vx_IrOp_add_out(add, ptr, vxcc_type(arr_expr->type));

            vx_IrOp* lod = vx_IrBlock_add_op_building(dest_block);
            vx_IrOp_init(lod, VX_IR_OP_LOAD, dest_block);
            vx_IrOp_add_param_s(lod, VX_IR_NAME_ADDR, (vx_IrValue) {.type = VX_IR_VAL_VAR, .var = ptr});
            vx_IrOp_add_out(lod, out, outTy);

            return VX_IRVAR_OPT_SOME(out);
        }

        default: {
            error_exit("expression type %i currenlty not supported by VXCC backend", expr->expr_kind);
            break;
        }
    }

    return VX_IRVAR_OPT_NONE;
}

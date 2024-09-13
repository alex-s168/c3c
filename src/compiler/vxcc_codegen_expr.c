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
        case BINARYOP_BIT_XOR:
        case BINARYOP_ELSE:
        case BINARYOP_ERROR: {
            error_exit("binary op %i not supported by VXCC backend", bin->operator);
            break;
        }

        case BINARYOP_MULT: ty = VX_IR_OP_MUL; break;
        case BINARYOP_SUB: ty = VX_IR_OP_SUB; break;
        case BINARYOP_ADD: ty = VX_IR_OP_ADD; break;
        case BINARYOP_DIV: ty = type_is_signed(expr->type) ? VX_IR_OP_SDIV : VX_IR_OP_UDIV; break;
        case BINARYOP_MOD: ty = VX_IR_OP_MOD; break;
        case BINARYOP_SHR: {
            if (type_is_signed(expr->type))
            {
                error_exit("signed shift right (ASHR) not yet supported by VXCC backend");
            }
            ty = VX_IR_OP_SHR;
            break; 
        }
        case BINARYOP_SHL: ty = VX_IR_OP_SHL; break;
        case BINARYOP_BIT_OR: ty = VX_IR_OP_BITIWSE_OR; break;
        case BINARYOP_BIT_AND: ty = VX_IR_OP_BITWISE_AND; break;
        case BINARYOP_AND: ty = VX_IR_OP_AND; break;
        case BINARYOP_OR: ty = VX_IR_OP_OR; break;
        case BINARYOP_GT: ty = type_is_signed(expr->type) ? VX_IR_OP_SGT : VX_IR_OP_UGT; break;
        case BINARYOP_GE: ty = type_is_signed(expr->type) ? VX_IR_OP_SGTE : VX_IR_OP_UGTE; break;
        case BINARYOP_LT: ty = type_is_signed(expr->type) ? VX_IR_OP_SLT : VX_IR_OP_ULT; break;
        case BINARYOP_LE: ty = type_is_signed(expr->type) ? VX_IR_OP_SLTE : VX_IR_OP_ULTE; break;
        case BINARYOP_NE: ty = VX_IR_OP_NEQ; break;
        case BINARYOP_EQ: ty = VX_IR_OP_EQ; break;

        case BINARYOP_CT_AND:
        case BINARYOP_CT_OR:
        case BINARYOP_CT_CONCAT:
        case BINARYOP_ASSIGN: 
        case BINARYOP_ADD_ASSIGN:
        case BINARYOP_BIT_AND_ASSIGN:
        case BINARYOP_BIT_OR_ASSIGN:
        case BINARYOP_BIT_XOR_ASSIGN:
        case BINARYOP_DIV_ASSIGN:
        case BINARYOP_MOD_ASSIGN:
        case BINARYOP_MULT_ASSIGN:
        case BINARYOP_SHR_ASSIGN:
        case BINARYOP_SHL_ASSIGN:
        case BINARYOP_SUB_ASSIGN: {
            error_exit("assignment binary ops not yet supported by VXCC backend");
            break;
        }
    }

    vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
    vx_IrOp_init(op, ty, dest_block);

    assert(outTy != NULL);
    vx_IrVar out = cu->nextVarId ++;
    vx_IrOp_add_out(op, out, outTy);

    vx_IrOp_add_param_s(op, VX_IR_NAME_OPERAND_A, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = right.var });
    vx_IrOp_add_param_s(op, VX_IR_NAME_OPERAND_B, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = left.var });

    return out;
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

        default: {
            error_exit("expression type %i currenlty not supported by VXCC backend", expr->expr_kind);
            break;
        }
    }

    return VX_IRVAR_OPT_NONE;
}

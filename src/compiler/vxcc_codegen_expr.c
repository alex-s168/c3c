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
            if (vd->bit_is_expr)
            {
                vx_OptIrVar init = vxcc_emit_expr(dest_block, cu, vd->init_expr);
                assert(init.present);

                vx_IrVar dest = vxcc_var(expr->decl_expr)->vxVar;

                vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
                vx_IrOp_init(op, VX_IR_OP_IMM, dest_block);
                vx_IrOp_add_param_s(op, VX_IR_NAME_VALUE, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = init.var });
                vx_IrOp_add_out(op, dest, vxcc_type(expr->decl_expr->type));

                return init;
            }
            else 
            {
                assert(false);
            }
            break;
        }

        case EXPR_POST_UNARY: {
            break; // TODO?
        }

        case EXPR_CAST: {
            vx_OptIrVar to_cast = vxcc_emit_expr(dest_block, cu, exprptr(expr->cast_expr.expr));
            assert(to_cast.present);
            switch (expr->cast_expr.kind)
            {
                // int size casts 
                case CAST_BOOLBOOL:
                case CAST_BOOLINT:
                case CAST_INTINT:
                case CAST_INTBOOL:
                case CAST_INTENUM: 
                case CAST_PTRBOOL:
                case CAST_PTRINT:
                case CAST_INTPTR:
                case CAST_PTRPTR: {
                    Type* old_ty = exprptr(expr->cast_expr.expr)->type; assert(old_ty);
                    Type* new_ty = expr->type;
                    
                    vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
                    
                    size_t old_si = type_size(old_ty);
                    size_t new_si = type_size(new_ty);
                    
                    vx_IrOpType op_kind;
                    if (old_si >= new_si)
                        op_kind = VX_IR_OP_BITCAST;
                    else if (type_is_signed(new_ty)) 
                        op_kind = VX_IR_OP_SIGNEXT;
                    else 
                        op_kind = VX_IR_OP_ZEROEXT;
                    
                    vx_IrOp_init(op, op_kind, dest_block);
                    vx_IrOp_add_param_s(op, VX_IR_NAME_VALUE, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = to_cast.var });
                    vx_IrVar out = cu->nextVarId ++;
                    vx_IrOp_add_out(op, out, vxcc_type(new_ty));
                    
                    return VX_IRVAR_OPT_SOME(out);
                }

                // int -> float casts
                case CAST_BOOLFP:
                case CAST_INTFP: {
                    vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
                    vx_IrOp_init(op, VX_IR_OP_TOFLT, dest_block);
                    vx_IrOp_add_param_s(op, VX_IR_NAME_VALUE, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = to_cast.var });
                    vx_IrVar out = cu->nextVarId ++;
                    vx_IrOp_add_out(op, out, vxcc_type(typeget(expr->cast_expr.type_info)));
                    break;
                }

                // float -> int casts 
                case CAST_FPBOOL:
                case CAST_FPINT: {
                    vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
                    vx_IrOp_init(op, VX_IR_OP_FROMFLT, dest_block);
                    vx_IrOp_add_param_s(op, VX_IR_NAME_VALUE, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = to_cast.var });
                    vx_IrVar out = cu->nextVarId ++;
                    vx_IrOp_add_out(op, out, vxcc_type(typeget(expr->cast_expr.type_info)));
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
                    error_exit("cast kind %i currenlty not supported by VXCC backend", expr->cast_expr.kind);
                    break;
                }
            }
            break;
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

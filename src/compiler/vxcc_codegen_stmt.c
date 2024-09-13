#include "vxcc_codegen_internal.h"

static void vxcc_emit_decl_stmt(vx_IrBlock* dest_block, VxccCU* cu, Decl* decl)
{
    switch (decl->decl_kind)
    {
        case DECL_VAR: {
            VarDecl* vd = &decl->var;
            VxccVarDecl* vxcc = vxcc_var(decl);
            vx_IrType* vxty = vxcc_type(typeget(decl->var.type_info));

            vx_IrValue initVal;
            if (vd->init_expr != NULL)
            {
                vx_OptIrVar initv = vxcc_emit_expr(dest_block, cu, vd->init_expr);
                if (!initv.present)
                {
                    error_exit("codegen error: expression assigned to varialbe does not return a value? (in declaration of var %s)", decl->name);
                }

                initVal.type = VX_IR_VAL_VAR;
                initVal.var = initv.var;
            }
            else 
            {
                initVal.type = VX_IR_VAL_UNINIT;
            }

            vx_IrOp* assign = vx_IrBlock_add_op_building(dest_block);
            vx_IrOp_init(assign, VX_IR_OP_IMM, dest_block);
            vx_IrOp_add_param_s(assign, VX_IR_NAME_VALUE, initVal);
            vx_IrOp_add_out(assign, vxcc->vxVar, vxty);

            break;
        }

        default: {
            error_exit("decl kind %i in function body currently not supported by VXCC backend", decl->decl_kind);
        }
    }
}

void vxcc_emit_stmt(vx_IrBlock* dest_block, VxccCU* cu, Ast* stmt)
{
    switch (stmt->ast_kind)
    {
        case AST_BLOCK_EXIT_STMT:
        case AST_POISONED: {
            // TODO?
            break;
        }

        case AST_ASM_STMT:
        case AST_ASM_BLOCK_STMT: {
            error_exit("inline ASM currently not supported by VXCC backend");
            break;
        }

        case AST_ASSERT_STMT: {
            // TODO 
            break;
        }

        case AST_NEXTCASE_STMT:
        case AST_CASE_STMT:
        case AST_SWITCH_STMT: 
        case AST_DEFAULT_STMT: {
            error_exit("switch / case / default currently not supported by VXCC backend");
            break;
        }

        case AST_DECLARE_STMT: {
            Decl* decl = stmt->declare_stmt;
            vxcc_emit_decl_stmt(dest_block, cu, decl);
            break;
        }

        case AST_RETURN_STMT: {
            Expr* expr = stmt->return_stmt.expr;
            vx_OptIrVar var = VX_IRVAR_OPT_NONE;
            if (expr != NULL)
            {
                var = vxcc_emit_expr(dest_block, cu, expr);
            }

            vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
            vx_IrOp_init(op, VX_IR_OP_RETURN, dest_block);
            if (var.present)
            {
                vx_IrOp_add_arg(op, (vx_IrValue) { .type = VX_IR_VAL_VAR, .var = var.var });
            }

            break;
        }

        case AST_DEFER_STMT: {
            error_exit("defer currently not supported by VXCC backend");
            break;
        }

        case AST_IF_STMT: {
            Expr* cond = exprptr(stmt->if_stmt.cond);
            Ast* bthen = astptr(stmt->if_stmt.then_body);
            Ast* belse = astptrzero(stmt->if_stmt.else_body);

            vx_IrOp* if_op = vx_IrBlock_add_op_building(dest_block);
            vx_IrOp_init(if_op, VX_IR_OP_IF, dest_block);

            vx_IrBlock* vbcond = vx_IrBlock_init_heap(dest_block, if_op);
            vx_OptIrVar condVar = vxcc_emit_expr(vbcond, cu, cond);
            assert(condVar.present);
            vx_IrBlock_add_out(vbcond, condVar.var);
            vx_IrOp_add_param_s(if_op, VX_IR_NAME_COND, (vx_IrValue) { .type = VX_IR_VAL_BLOCK, .block = vbcond });

            vx_IrBlock* vbthen = vx_IrBlock_init_heap(dest_block, if_op);
            for (; bthen; bthen = astptrzero(bthen->next))
            {
                vxcc_emit_stmt(vbthen, cu, bthen);
            }
            vx_IrOp_add_param_s(if_op, VX_IR_NAME_COND_THEN, (vx_IrValue) { .type = VX_IR_VAL_BLOCK, .block = vbthen });

            vx_IrBlock* vbelse = vx_IrBlock_init_heap(dest_block, if_op);
            // can be empty 
            for (; belse; belse = astptrzero(belse->next))
            {
                vxcc_emit_stmt(vbelse, cu, belse);
            }
            vx_IrOp_add_param_s(if_op, VX_IR_NAME_COND_ELSE, (vx_IrValue) { .type = VX_IR_VAL_BLOCK, .block = vbelse });

            break;
        }

        case AST_FOR_STMT:
        case AST_FOREACH_STMT:
        case AST_IF_CATCH_SWITCH_STMT: {
            error_exit("statement currently not supported by VXCC backend in: %s", span_to_string(stmt->span));
            break;
        }

        case AST_DECLS_STMT: {
            Decl** decls = stmt->decls_stmt;
            FOREACH(Decl *, decl, decls)
            {
                vxcc_emit_decl_stmt(dest_block, cu, decl);
            }
            break;
        }

        case AST_COMPOUND_STMT: {
            Ast* block = astptrzero(stmt->compound_stmt.first_stmt);
            for (; block; block = astptrzero(block->next))
            {
                vxcc_emit_stmt(dest_block, cu, block);
            }
            break;
        }

        case AST_CONTINUE_STMT:
        case AST_BREAK_STMT: {
            if (stmt->contbreak_stmt.is_label)
            {
                error_exit("label continues and breaks currently not supported by VXCC backend");
            }

            Ast* defers = astptrzero(stmt->contbreak_stmt.defers);
            if (defers)
            {
                vxcc_emit_stmt(dest_block, cu, defers);
            }

            vx_IrOpType opTy = VX_IR_OP_BREAK;
            if (stmt->ast_kind == AST_CONTINUE_STMT)
            {
                opTy = VX_IR_OP_CONTINUE;
            }

            vx_IrOp* op = vx_IrBlock_add_op_building(dest_block);
            vx_IrOp_init(op, opTy, dest_block);

            break;
        }

        case AST_EXPR_STMT: {
            (void) vxcc_emit_expr(dest_block, cu, stmt->expr_stmt);
            break;
        }

        case AST_NOP_STMT: break;

        case AST_CT_ASSERT:
        case AST_CT_IF_STMT:
        case AST_CT_ECHO_STMT:
        case AST_CT_FOR_STMT:
        case AST_CT_ELSE_STMT:
        case AST_CT_SWITCH_STMT:
        case AST_CT_FOREACH_STMT:
        case AST_CONTRACT:
        case AST_CONTRACT_FAULT:
               break;
    }
}

#include "compiler_internal.h"
#include "../../vxcc/ir/ir.h"
#include "../../vxcc/ir/opt.h"

// if the function returns a value (not void), the variable that contains the return type is always 0
// variables are enumerated trough the whole CU and can be any id except 0 
// note that none of these variables are actually global! they all are only valid in some function; they are numerated across the whole CU for simplicity

/** VarDecl.optional_ref can be casted to a pointer of this */
typedef struct {
    vx_IrVar vxVar;
} VxccVarDecl;

/** CompilationUnit.vxcc can be casted to a pointer of this */
typedef struct {
    vx_IrVar nextVarId;
} VxccCU;

module Generate.Util where

import LLVM.Simple
import LLVM.Types
import LLVM.Values

mallocSz :: ValueRef -> Build ValueRef
mallocSz = call "GC_malloc" . (:[])

mallocTyp :: TypeRef -> Build ValueRef
mallocTyp t = do
    sz <- sizeOf t
    pt <- mallocSz sz
    bitcast pt (pointerType t 0) "casting char ptr to typed ptr"

sizeOf :: TypeRef -> Build ValueRef
sizeOf t = do
  let pt = pointerType t 0
  ptr <- gep (nul pt) [int 1]
  ptrToInt ptr int32Type "convert pointer to size"

arrayMalloc :: ValueRef -> TypeRef -> Build ValueRef
arrayMalloc i t = do
  st <- sizeOf t
  arSize <- mul st i "multiply number of elements"
  ptr <- mallocSz arSize
  bitcast ptr (pointerType t 0) "casting ptr to array"

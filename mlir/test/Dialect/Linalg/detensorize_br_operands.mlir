// RUN: mlir-opt %s -split-input-file -allow-unregistered-dialect -linalg-detensorize | FileCheck %s

// TODO: Detensoring breaks if %arg0 or %arg1 are passed directly as tensors. Fix that.
func @if_true_test(%arg0: i1, %arg1: i32) -> tensor<i32> attributes {} {
  %arg0_t = tensor.from_elements %arg0 : tensor<1xi1>
  %arg0_t2 = tensor.collapse_shape %arg0_t [] : tensor<1xi1> into tensor<i1>

  %arg1_t = tensor.from_elements %arg1 : tensor<1xi32>
  %arg1_t2 = tensor.collapse_shape %arg1_t [] : tensor<1xi32> into tensor<i32>

  %cst = arith.constant dense<10> : tensor<i32>
  %2 = linalg.init_tensor [] : tensor<i8>
  %3 = linalg.generic
    {indexing_maps = [affine_map<() -> ()>, affine_map<() -> ()>], iterator_types = []}
    ins(%arg0_t2 : tensor<i1>)
    outs(%2 : tensor<i8>) {
  ^bb0(%arg2: i1, %arg3: i8):  // no predecessors
    %10 = arith.extui %arg2 : i1 to i8
    linalg.yield %10 : i8
  } -> tensor<i8>
  %4 = tensor.extract %3[] : tensor<i8>
  %5 = arith.trunci %4 : i8 to i1
  cond_br %5, ^bb1, ^bb2(%arg1_t2 : tensor<i32>)
^bb1:
  %6 = linalg.init_tensor [] : tensor<i32>
  %7 = linalg.generic
    {indexing_maps = [affine_map<() -> ()>, affine_map<() -> ()>, affine_map<() -> ()>], iterator_types = []}
    ins(%arg1_t2, %cst : tensor<i32>, tensor<i32>)
    outs(%6 : tensor<i32>) {
  ^bb0(%arg2: i32, %arg3: i32, %arg4: i32):  // no predecessors
    %10 = arith.addi %arg2, %arg3 : i32
    linalg.yield %10 : i32
  } -> tensor<i32>
  br ^bb2(%7 : tensor<i32>)
^bb2(%8: tensor<i32>):
  return %8 : tensor<i32>
}

// CHECK-LABEL:  func @if_true_test
// CHECK-SAME:     (%[[arg0:.*]]: i1, %[[arg1:.*]]: i32)
// CHECK-NEXT:     arith.constant 10 : i32
// CHECK-NEXT:     cond_br %[[arg0]], ^[[bb1:.*]], ^[[bb2:.*]](%[[arg1]] : i32)
// CHECK-NEXT:   ^[[bb1]]:
// CHECK-NEXT:     %[[add_res:.*]] = arith.addi
// CHECK-NEXT:     br ^[[bb2]](%[[add_res]] : i32)
// CHECK-NEXT:   ^[[bb2]]
// CHECK-NEXT:     tensor.from_elements
// CHECK-NEXT:     %[[func_res:.*]] = tensor.collapse_shape
// CHECK-NEXT:     return %[[func_res]]

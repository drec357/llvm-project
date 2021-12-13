#!/bin/bash
SRC_DIR=$(realpath "$(dirname ${0})")
PREFIX=${1:-/opt/llvm/install}
for src in "${SRC_DIR}/"*.cpp
do
	echo $(basename "${src}")
	"${PREFIX}/bin/clang++" \
		-std=c++20  -stdlib=libc++ \
		-freflection-ts -freflection-ext \
		-isystem "${PREFIX}/include/c++/v1" \
		-isystem "${PREFIX}/lib/clang/14.0.0/include" \
		-o "${SRC_DIR}/test" \
		"${src}" && \
	(export "LD_LIBRARY_PATH=${PREFIX}/lib" && "${SRC_DIR}/test")
done

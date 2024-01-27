RELEASE="--release"

if [ "$1" = "debug" ]; then
  RELEASE=""
fi

TARGET_DIR=$(pwd)/target

# install wasm32-wasi target
rustup target add wasm32-wasi

cd src/core
CARGO_TARGET_DIR=${TARGET_DIR} cargo build --target wasm32-wasi ${RELEASE}

cd ../../
cargo build ${RELEASE}

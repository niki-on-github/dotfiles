#!/bin/bash

readonly filename=$(readlink -f "${1}")
readonly basename=$(basename "${filename}")
readonly input_dir=$(dirname "${filename}")/
readonly wrapper_dir=$(dirname "${0}")

if ! docker images | grep -q pandoc-eisvogel; then
    pushd $wrapper_dir
    docker build -t pandoc-eisvogel .
    popd
fi

docker run \
    --rm \
    --user $(id -u):$(id -g) \
    --volume "${input_dir}:/data" \
    pandoc-eisvogel \
        "/data/${basename}" \
        --from=markdown \
        --data-dir=/opt/pandoc \
        --filter pandoc-plantuml \
        --filter pandoc-crossref \
        --filter pandoc-include \
        --filter mermaid-filter \
        --template eisvogel \
        -o ${basename%.*}.pdf

if [ ! -s $input_dir/mermaid-filter.err ]; then
    rm -rf $input_dir/mermaid-filter.err
fi

rm -rf $input_dir/?

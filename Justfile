format:
    roc format

check:
    roc check src/main.roc

test:
    roc test src/main.roc

build *ARGS:
    roc build --optimize {{ARGS}} src/main.roc

build-bundle *ARGS:
    just build --bundle .tar.br {{ARGS}}

run *ARGS:
    roc dev src/main.roc -- {{ARGS}}

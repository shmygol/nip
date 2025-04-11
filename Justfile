format:
    roc format

check:
    roc check src/main.roc

test:
    roc test src/main.roc

run *ARGS:
    roc dev src/main.roc -- {{ARGS}}

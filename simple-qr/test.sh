HOME_DIR="racket-simple-qr/simple-qr"
echo "func";cd; cd $HOME_DIR/lib/func;racket func-test.rkt
echo "func/capacity_table";cd; cd $HOME_DIR/lib/func/capacity;racket capacity-test.rkt
echo "timing-pattern";cd; cd $HOME_DIR/lib/timing-pattern;racket timing-pattern-test.rkt
echo "alignment-pattern";cd; cd $HOME_DIR/lib/alignment-pattern;racket alignment-pattern-test.rkt


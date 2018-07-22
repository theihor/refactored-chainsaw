make -f makefile all
#TODO: Add option to main
for f in ./problems/FA/*.mdl; do
    ./main -f "$f"
done
for f in ./problems/FD/*.mdl; do
    ./main -f "$f"
done
#TODO: Add FR run because two models should be passed to the ./main
for f in ./problems/FR/*.mdl; do
    echo "$f"
    #./main -f "$f"
done


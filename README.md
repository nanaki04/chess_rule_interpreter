# chess_rule_interpreter
WIP

Build command:

```
$ swipl --goal=main --stand_alone=true -o chess -c chess.pl
```

Sample command:

```
$ ./chess [[[0,[4,0],\"white\",\"king\",0,true,[[0,0],0]],[1,[7,0],\"white\",\"rook\",0,true,[[0,0],0]],[2,[5,1],\"white\",\"knight\",0,true,[[0,0],0]],[3,[6,7],\"black\",\"rook\",1,true,[[0,0],0]]],64,0] assess_moves [4,0]
```

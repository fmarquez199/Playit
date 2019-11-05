from pathlib import Path
from os.path import join
import os

os.system("stack build")

def createTests(comp):
    test_todos = Path(join('test','casos',comp)).glob('**/*.game')

    test_files = [filename for filename in test_todos]

    n_todos = len(test_files)

    i = 1

    for filename in test_files:
        filegame = open(filename,"r")

        command_runtest = 'stack run ' + filename.as_posix()
        output = os.popen(command_runtest).read()
        
        finameout = filename.as_posix().split(filename.suffix)[0] + ".out" + comp
        fileout  = open(finameout,"w")
        fileout.write(output)
        filegame.close()
        fileout.close()
        print("Wrote: " + filename.as_posix() + " to " + finameout)
        print(str(round(i/n_todos,2) * 100) + " % completed...")
        i += 1


# createTests("parser")
createTests("symtab")


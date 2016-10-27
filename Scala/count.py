#!/usr/bin/env python3

from pathlib import *
import subprocess
import csv

NAMES = ["Arith", "Untyped", "FullUntyped", "TyArith", "SimpleBool",
         "FullSimple", "Bot", "FullRef", "FullError", "RcdSubBot", "FullSub",
         "FullEquiRec", "FullIsoRec", "EquiRec", "Recon", "FullRecon",
         "FullPoly", "FullOmega"]


def count_theirs():
    print('COUNTING THEIR LIBRARY')
    ret = []
    root = Path('Parser/src/TAPLcomp/')
    for name in NAMES:
        p = str(root / name.lower())
        result = subprocess.run(["cloc", "--by-file", "--csv", "--quiet", p],
                                stdout=subprocess.PIPE)
        result.check_returncode()
        res_csv = result.stdout.decode().strip().split("\n")[1:]
        res = sum([int(r.split(',')[-1]) for r in res_csv])
        print(p, res)
        ret.append(res)
    return ret


def count_ours():
    print('COUNTING OUR LIBRARY')
    ret = []
    root = Path('Parser/src/TAPL/')
    for name in NAMES:
        p = str(root / (name + ".scala"))
        result = subprocess.run(["cloc", "--by-file", "--csv", "--quiet", p],
                                stdout=subprocess.PIPE)
        result.check_returncode()
        res_csv = result.stdout.decode().strip().split("\n")[1]
        res = int(res_csv.split(',')[-1])
        print(p, res)
        ret.append(res)
    return ret


def main():
    theirs = count_theirs()
    ours = count_ours()
    assert(len(theirs) == len(ours) == len(NAMES))

    def cal_percent(old, new):
        per_str = "{:+.1%}".format((new - old) / old)
        return per_str

    FILE_NAME = 'sloc.csv'
    print('WRITING RESULT TO', FILE_NAME)
    with open(FILE_NAME, 'w') as csvfile:
        field_names = ['Name', 'Their Lib', 'Our Lib', "+/- %"]
        writer = csv.DictWriter(csvfile, fieldnames=field_names)
        writer.writeheader()
        for (name, old, new) in zip(NAMES, theirs, ours):
            vals = [name, old, new, cal_percent(old, new)]
            print(vals)
            row = dict(zip(field_names, vals))
            writer.writerow(row)
        # write the total sum
        s1, s2 = sum(theirs), sum(ours)
        vals = ['Total', s1, s2, cal_percent(s1, s2)]
        writer.writerow(dict(zip(field_names, vals)))
    print('DONE')


if __name__ == '__main__':
    main()

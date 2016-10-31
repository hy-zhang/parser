#!/usr/bin/env python3

from pathlib import *
import os
import csv
import subprocess

NAMES = ["Arith", "Untyped", "FullUntyped", "TyArith", "SimpleBool",
         "FullSimple", "Bot", "FullRef", "FullError", "RcdSubBot", "FullSub",
         "FullEquiRec", "FullIsoRec", "EquiRec", "Recon", "FullRecon",
         "FullPoly", "FullOmega"]


def print_hline():
    print('=' * 40)


def count_theirs():
    print_hline()
    print('COUNTING THEIR LIBRARY')
    print_hline()
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
    print_hline()
    print('COUNTING OUR LIBRARY')
    print_hline()
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


def time(test_class):
    ret = []
    for name in NAMES:
        result = subprocess.run(
                    ["scala", "-cp", "bin", test_class, name],
                    stdout=subprocess.PIPE)
        result.check_returncode()
        res = int(result.stdout.decode().strip().split('\n')[-1])
        ret.append(res)
        print(name, res)
    return ret


def time_theirs():
    print_hline()
    print('TIMING THEIR LIBRARY')
    print_hline()
    return time('TAPLcomp.Test')


def time_ours():
    print_hline()
    print('TIMING OUR LIBRARY')
    print_hline()
    return time('TAPL.Test.Test')


def write_csv(sloc1, sloc2, time1, time2):
    def cal_percent(old, new):
        per_str = "{:+.1%}".format((new - old) / old)
        return per_str
    # write each case
    file_name = 'table.csv'
    print_hline()
    print('WRITING RESULT TO', file_name)
    print_hline()
    with open(file_name, 'w') as csvfile:
        field_names = [
            'name', 'theirsloc', 'oursloc', 'slocdiff',
            'theirtime', 'ourtime', 'timediff']
        writer = csv.DictWriter(csvfile, fieldnames=field_names)
        writer.writeheader()
        for (name, s1, s2, t1, t2) in zip(NAMES, sloc1, sloc2, time1, time2):
            vals = [name,
                    s1, s2, cal_percent(s1, s2),
                    t1, t2, cal_percent(t1, t2)]
            print(vals)
            row = dict(zip(field_names, vals))
            writer.writerow(row)
    # write total
    file_name = 'table_total.csv'
    with open(file_name, 'w') as csvfile:
        writer = csv.writer(csvfile)
        s1, s2, t1, t2 = sum(sloc1), sum(sloc2), sum(time1), sum(time2)
        row = [
            'Total',
            s1, s2, cal_percent(s1, s2),
            t1, t2, cal_percent(t1, t2)]
        print(row)
        writer.writerow(row)


def write_tex(file_name, sloc1, sloc2, time1, time2):
    def cal_percent(old, new):
        per_str = "{:+.1f}".format((new - old) / old * 100)
        return per_str

    def tex_line(name, s1, s2, t1, t2):
        vals = [name,
                s1, s2, cal_percent(s1, s2),
                t1, t2, cal_percent(t1, t2)]
        return " & ".join([str(v) for v in vals]) + " \\\\\n"

    # write each case
    print_hline()
    print('WRITING RESULT TO', file_name)
    print_hline()
    with open(file_name, 'w') as f:
        for (name, s1, s2, t1, t2) in zip(NAMES, sloc1, sloc2, time1, time2):
            row = tex_line(name, s1, s2, t1, t2)
            print(row, end='')
            f.write(row)
        # write total
        s1, s2, t1, t2 = sum(sloc1), sum(sloc2), sum(time1), sum(time2)
        row = tex_line('Total', s1, s2, t1, t2)
        print(row, end='')
        f.write('\\hline\n')
        f.write(row)


def main():
    # count SLOC
    sloc1 = count_theirs()
    sloc2 = count_ours()
    assert(len(sloc1) == len(sloc2) == len(NAMES))

    # calculate execution time
    os.chdir('Parser')
    print('Change working directory to', os.getcwd())
    time1 = time_theirs()
    time2 = time_ours()
    os.chdir('..')
    print('Change working directory to', os.getcwd())
    assert(len(time1) == len(time2) == len(NAMES))

    # write to file
    # write_csv(sloc1, sloc2, time1, time2)
    tex_file = '../paper/resources/CaseStudyTable.tex'
    write_tex(tex_file, sloc1, sloc2, time1, time2)


if __name__ == '__main__':
    main()

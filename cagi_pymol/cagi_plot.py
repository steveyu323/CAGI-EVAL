from pymol import cmd, stored
import csv

'''
The function takes in an array with columns that a position and
how good that position is predicted by a method/a group method
'''

path = "../GAA/5nn3.csv"
def PlotCorrectness(prot_name,path):
    fields = []
    rows = []
    with open(path, 'r') as csvfile:
        # creating a csv reader object
        csvreader = csv.reader(csvfile,quoting=csv.QUOTE_NONNUMERIC)
        # extracting field names through first row
        fields = csvreader.next()
        # fields = next(csvreader)
        print(fields)
        # extracting each data row one by one
        for row in (csvreader):
            rows.append(row)
    value_ls = [row[2] for row in rows]
    res_ls = [row[1] for row in rows]

    cmd.fetch (prot_name)
    cmd.hide(representation = "",selection = "all")
    cmd.show("cartoon", "all")
    util.chainbow(prot_name)
    cmd.set(name = "cartoon_transparency", value = "0.6", selection = "all")
    for i in range(len(res_ls)):
        # extract index and the corresponding predictability
        i_resi = "resi" + " " + str(res_ls[i])
        i_color = value_ls[i]
        # [red green blue]
        color_array = [i_color, 1.00 - i_color, 0.00]
        cmd.set_color(name = "current" + str(i), rgb = color_array)
        cmd.show(representation="surface", selection= i_resi )
        # cmd.show(representation="stick", selection= i_resi )
        cmd.color(color = "current" + str(i), selection = i_resi)
    filename = "./" + prot_name + ".pse"
    cmd.save(filename)
    cmd.extend( "PlotCorrectness", PlotCorrectness );

import os

with open('convert_list.txt', 'r') as file:
    for line in file:
        splitted = line.strip().split(' ')
        (after, before) = (splitted[0], splitted[1])
        # print(after, before)

        print(f'./convert.sh {before}') 
        os.system(f'./convert.sh {before}') 

        print(f'mv {before}.eps Fig{after}.eps')
        os.system(f'mv {before}.eps Fig{after}.eps')

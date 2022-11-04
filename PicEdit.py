#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 23 12:32:39 2020

@author: donovan
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as mpimg

# load image
def load_image(filename):
    img = mpimg.imread(filename)
    if len(img[0][0])==4: # if png file
        img = np.delete(img, 3, 2)
    if type(img[0][0][0])==np.float32:  # if stored as float in [0,..,1] instead of integers in [0,..,255]
        img = img*255
        img = img.astype(np.uint8)
    mask = np.ones((len(img),len(img[0]))) # create a mask full of "1" of the same size of the laoded image
    img = img.astype(np.int32)
    return img, mask

def user_load():
    file = ''
    ask = True
    while ask:
        try:
            file = input('Enter filename: ')
            img , mask = load_image(file)
            ask = False
        except:
            print('File not found! Please try again!')
    return img,mask

def display_image(image, mask):
    # if using Spyder, please go to "Tools -> Preferences -> IPython console -> Graphics -> Graphics Backend" and select "inline"
    tmp_img = image.copy()
    edge = compute_edge(mask)
    for r in range(len(image)):
        for c in range(len(image[0])):
            if edge[r][c] == 1:
                tmp_img[r][c][0]=255
                tmp_img[r][c][1]=0
                tmp_img[r][c][2]=0
 
    plt.imshow(tmp_img)
    plt.axis('off')
    plt.show()
    print("Image size is",str(len(image)),"x",str(len(image[0])))
    
def save_image(filename, image):
    img = image.astype(np.uint8)
    mpimg.imsave(filename,img)
    
def user_save(img):
    file = ''
    ask = True
    while ask:
        try:
            file = input('*** Note: End filename with .jpg or .png ***\
                         \nEnter filename to be saved as: ')
            save_image(file, img)
            ask = False
        except:
            print('Error!')

a = 1

def compute_edge(mask):           
    rsize, csize = len(mask), len(mask[0]) 
    edge = np.zeros((rsize,csize))
    if np.all((mask == 1)): return edge
    for r in range(rsize):
        for c in range(csize):
            if mask[r][c]!=0:
                if r==0 or c==0 or r==len(mask)-1 or c==len(mask[0])-1:
                    edge[r][c]=1
                    continue
                
                is_edge = False                
                for var in [(-1,0),(0,-1),(0,1),(1,0)]:
                    r_temp = r+var[0]
                    c_temp = c+var[1]
                    if 0<=r_temp<rsize and 0<=c_temp<csize:
                        if mask[r_temp][c_temp] == 0:
                            is_edge = True
                            break
    
                if is_edge == True:
                    edge[r][c]=1
            
    return edge

def change_brightness(image,value):
    dup = image.copy()
    for r , row in enumerate(dup):
        for c , col in enumerate(row):
            for index , rgb in enumerate(col):
                v = dup[r][c][index]
                if v + value > 255:
                    v = 255
                elif v + value < 0:
                    v = 0
                else:
                    v += value
                dup[r][c][index] = v
    return dup

def user_change_brightness(image):
    value = 0
    ask = True
    while ask:
        value = int(input('Enter brightness values: '))
        if -255 <= value <= 255:
            newimg = change_brightness(image,value)
            ask = False
        else:
            print('Error! Please enter a valid value!')
            continue
    return (newimg)

def change_contrast(image, value):
    dup = image.copy()
    numerator = 259 * (value + 255)
    denominator = 255 * (259 - value)
    F = numerator / denominator
    for r , row in enumerate(dup):
        for c , col in enumerate(row):
            for index , rgb in enumerate(col):
                rgb -= 128
                rgb *= F
                rgb += 128
                if rgb > 255:
                    rgb = 255
                elif rgb < 0:
                    rgb = 0
                else:
                    pass
                dup[r][c][index] = rgb
    return dup

def user_change_contrast(image):
    value = 0
    ask = True
    while ask:
        value = int(input('Enter contrast values: '))
        if -255 <= value <= 255:
            newimg = change_contrast(image,value)
            ask = False
        else:
            print('Error! Please enter a valid value!')
            continue
    return newimg

def grayscale (image):
    dup = image.copy()
    for r , row in enumerate(dup):
        for c , col in enumerate(row):
            red = dup[r][c][0]
            green = dup[r][c][1]
            blue = dup[r][c][2]
            v = (0.3 * red) + (0.59 * green) + (0.11 * blue)
            for index , rgb in enumerate(col):
                dup[r][c][index] = v
    return dup

def find_matrix_M(image, r , c):
    matrix_M = []
    for i in range(-1, 2):
        for j in range(-1, 2):
            matrix_M.append(image[r+i][c+j])
    return matrix_M

def blur_effect(image):
    
    dup = image.copy()
    
    for r , row in enumerate(image):
        if r == 0 or r == len(image) - 1:
            continue
        else:
            pass
            
        for c , col in enumerate(row):
            if c == 0 or c == len(row) - 1:
                continue
            else:
                pass
            
            for index , rgb in enumerate(col):
                p_prime = [0,0,0]
                matrix_M = find_matrix_M(image, r , c)
                matrix_K = [0.0625, 0.125, 0.0625, 0.125, 0.25, 0.125, 0.0625, 0.125, 0.0625]
                for values in range(len(matrix_M)):
                    for v in range(3):
                        p_prime[v] += (matrix_M[values][v] * matrix_K[values])
                dup[r][c] = p_prime      
    return dup # topbe removed when filling this function

def edge_detection (image):
    
    dup = image.copy()
    
    for r , row in enumerate(image):
        if r == 0 or r == len(image) - 1:
            continue
        else:
            pass
            
        for c , col in enumerate(row):
            if c == 0 or c == len(row) - 1:
                continue
            else:
                pass
            
            for index , rgb in enumerate(col):
                p_prime = [0,0,0]
                matrix_M = find_matrix_M(image, r , c)
                matrix_K = [-1, -1, -1, -1, 8, -1, -1, -1, -1]
                for values in range(9):
                    for v in range(3):
                        p_prime[v] += (matrix_M[values][v] * matrix_K[values])
            for i in range(3):
                p_prime[i] += 128
                if p_prime[i] > 255:
                    p_prime[i] = 255
                elif p_prime[i] < 0:
                    p_prime[i] = 0
                else:
                    continue
            dup[r][c] = p_prime
    return dup # topbe removed when filling this function

def embossed (image):
    dup = image.copy()
    
    for r , row in enumerate(image):
        if r == 0 or r == len(image) - 1:
            continue
        else:
            pass
            
        for c , col in enumerate(row):
            if c == 0 or c == len(row) - 1:
                continue
            else:
                pass
            
            for index , rgb in enumerate(col):
                p_prime = [0,0,0]
                matrix_M = find_matrix_M(image, r , c)
                matrix_K = [-1, -1, 0, -1, 0, 1, 0, 1, 1]
                for values in range(9):
                    for v in range(3):
                        p_prime[v] += (matrix_M[values][v] * matrix_K[values])
            for i in range(3):
                p_prime[i] += 128
                if p_prime[i] > 255:
                    p_prime[i] = 255
                elif p_prime[i] < 0:
                    p_prime[i] = 0
                else:
                    continue
            dup[r][c] = p_prime
    return dup # topbe removed when filling this function

def user_rectangle_select (image):
    row = len(image)
    col = len(image[0])

    ask = True
    topleftrow = ''
    topleftcolumn = ''
    bottomrightrow = ''
    bottomrightcolumn = ''
    while ask:
        while topleftrow == '':
            topleftrow = int(input('Enter top left row: '))
            # excludes last row as well
            if topleftrow < 0 or topleftrow >= row - 1:
                print('Error: Enter correct value')
                topleftrow = ''
            else:
                pass
            
        while topleftcolumn == '':
            topleftcolumn = int(input('Enter top left column: '))
            # excludes last column as well
            if topleftcolumn < 0 or topleftcolumn >= col - 1:
                print('Error: Enter correct value')
                topleftcolumn = ''
            else:
                pass
            
        while bottomrightrow == '':
            bottomrightrow = int(input('Enter bottom right row: '))
            if bottomrightrow <= topleftrow or bottomrightrow > row - 1:
                print('Error: Enter correct value')
                bottomrightrow = ''
            else:
                pass
        
        while bottomrightcolumn == '':    
            bottomrightcolumn = int(input('Enter bottom right column: '))
            if bottomrightcolumn <= topleftcolumn or bottomrightcolumn > col - 1:
                print('Error: Enter correct value')
                bottomrightcolumn = ''
            else:
                pass
        
        ask = False
        
    topleft = (topleftrow , topleftcolumn)
    bottomright = (bottomrightrow , bottomrightcolumn)
    return topleft , bottomright

def rectangle_select (image , topleft , bottomright):
    
    topleftrow , topleftcolumn = topleft
    bottomrightrow , bottomrightcolumn = bottomright

    row = len(image)
    col = len(image[0])
    
    mask = []
    
    for r in range(row):
        mask.append([])
    for r in range(row):
        for c in range(col):
            mask[r].append([])

    for i in range(row):
        if i not in range(topleftrow,bottomrightrow+1):
            for j in range(col):
                mask[i][j] = 0
        else:
            for j in range(col):
                mask[i][j] = 1
    
    for j in range(col):
        if j not in range(topleftcolumn,bottomrightcolumn+1):
            for i in range(row):
                mask[i][j] = 0
    return mask

def color_distance(pixel , pixel1):
    red = pixel[0]
    green = pixel[1]
    blue = pixel[2]
    
    red1 = pixel1[0]
    green1 = pixel1[1]
    blue1 = pixel1[2]
    
    rdiff = abs(red - red1)
    gdiff = abs(green - green1)
    bdiff = abs(blue - blue1)
    
    r = (red + red1) / 2
    
    distance = 2 + r/256
    distance *= (rdiff**2)
    distance += (4 * (gdiff ** 2))
    distance += (2 + (255 - r) / 256) * (bdiff ** 2)
    distance = distance ** 0.5
    
    return distance

def check_neighbour(image , x):
    rownum , colnum = x
    
    row = len(image)
    col = len(image[0])
    
    pixel_list = []
    
    if rownum > 0:
        toppixel = image[rownum-1][colnum]
        data = [rownum-1 , colnum , toppixel]
        pixel_list.append(data)
    if rownum < (row-1):
        bottompixel = image[rownum+1][colnum]
        data = [rownum+1 , colnum , bottompixel]
        pixel_list.append(data)
    if colnum > 0:
        leftpixel = image[rownum][colnum-1]
        data = [rownum , colnum-1 , leftpixel]
        pixel_list.append(data)
    if colnum < (col-1):
        rightpixel = image[rownum][colnum+1]
        data = [rownum , colnum+1 , rightpixel]
        pixel_list.append(data)
        
    return pixel_list

def user_magic_wand_select(image):
    row_len = len(image)
    col_len = len(image[0])
    r = -1
    c = -1
    thres = ''
    while r < 0 or r >= row_len:
        r = int(input('Enter row position: '))
    while c < 0 or c >= col_len:
        c = int(input('Enter col position: '))
    while type(thres) != int:
        thres = int(input('Enter threshold value: '))
    
    x = (r,c)
    return x , thres
    
def magic_wand_select(image, x, thres):
    
    rownum , colnum = x
    row_len = len(image)
    col_len = len(image[0])
    pixel = image[rownum][colnum]
    
    mask = []
    for r in range(row_len):
        mask.append([])
    for r in range(row_len):
        for c in range(col_len):
            mask[r].append(2)
    
    mask[rownum][colnum] = 1
    
    to_check = []
    checked = []
    to_check.append(x)
    
    while len(to_check) > 0:
        position = to_check[0]
        pixellist = check_neighbour(image,position)
        
        for index,data in enumerate(pixellist):
            row = data[0]
            col = data[1]
            rgb = data[2]
            position_vector = (row,col)
            distance = color_distance(pixel,rgb)
            
            if distance <= thres:
                mask[row][col] = 1
                if position_vector not in checked:
                    to_check.append(position_vector)
                else:
                    pass
            else:
                mask[row][col] = 0
                
            checked.append(position_vector)
        
        if len(to_check) > 1:
            to_check = to_check[1:]
        else:
            to_check = []
    
    for r in range(row_len):
        for c in range(col_len):
            if mask[r][c] == 2:
                mask[r][c] = 0
                
    return mask

# User Interface
MENU = 'What do you want to do ?\
        \ne - exit\
        \nl - load a picture\
        \ns - save the current picture\
        \n1 - adjust brightness\
        \n2 - adjust contrast\
        \n3 - apply grayscale\
        \n4 - apply blur\
        \n5 - edge detection\
        \n6 - embossed\
        \n7 - rectangle select\
        \n8 - magic wand select'

IMAGE = 'What do you want to do ?\
        \ne - exit\
        \nl - load a picture'

def menu():
    
    choice = ''
    
    while choice == '':
        
        print (IMAGE)
        choice = input('Your choice:')
        
        if choice == 'e':
            # exit
            break
        elif choice == 'l':
            # load image
            print ('load') 
            img,mask = user_load()
            working_image = img.copy()
            working_mask = mask.copy()
        else:
            print ('Error! Try again!')
            choice = ''

        
    while choice != 'e':
        
        preimg = working_image.copy()
        display_image(preimg,working_mask)
        print (MENU)
        choice = input('Your choice:')
            
        if choice == 'e':
            break
        elif choice == 'l':
            img,mask = user_load()
            working_image = img.copy()
            working_mask = mask.copy()
        elif choice == 's':
            user_save(working_image)
        elif choice == '1':
            working_image = user_change_brightness(working_image)
            print('bright photo')
            display_image(working_image,working_mask)
        elif choice == '2':
            working_image = user_change_contrast(working_image)
            display_image(working_image,working_mask)
        elif choice == '3':
            working_image = grayscale(working_image)
            display_image(working_image,working_mask)
        elif choice == '4':
            working_image = blur_effect(working_image)
            display_image(working_image,working_mask)
        elif choice == '5':
            working_image = edge_detection(working_image)
            display_image(working_image,working_mask)
        elif choice == '6':
            working_image = embossed(working_image)
            display_image(working_image,working_mask)
        elif choice == '7':
            topleft , bottomright = user_rectangle_select(working_image)
            working_mask = rectangle_select(working_image,topleft,bottomright)
            print(working_mask)
        elif choice == '8':
            position , threshold_value = user_magic_wand_select(working_image)
            working_mask = magic_wand_select(working_image,position,threshold_value)
        else:
            print('Invalid choice. Please try again!')
        
        newimg = working_image.copy()
        for r,row in enumerate(newimg):
            for c,col in enumerate(newimg[r]):
                if working_mask[r][c] == 1:
                    newimg[r][c] = working_image[r][c]
                else:
                    newimg[r][c] = preimg[r][c]
        working_image = newimg
        

        
            
            
            
            
        
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
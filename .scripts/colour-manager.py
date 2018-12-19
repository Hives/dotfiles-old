import os
import pprint
import subprocess
import sys
from pathlib import Path
from shutil import copyfile

# sys.argv is the list of arguments
# the first (zeroth) argument is always the name of the script
if len(sys.argv) > 1:
    scheme = sys.argv[1]
else:
    scheme = "dark-scheme"

home = str(Path.home()) + "/"
path = home + ".config/xcolors/"
schemefile = path + scheme

print ('scheme file: ' + schemefile)

def update_config ( path, name, output ):
    "Replaces anything between the two markers in 'configfile' with 'output'"

    config_file = path + name
    tmp_file = path + name + "-tmp"

    copying = True
    colours_ended = False
    with open(config_file) as old_file, open(tmp_file, "w") as new_file:
        for line in old_file:
           if not copying and "ʕっ•ᴥ•ʔっ COLOURS END" in line:
               copying = True
               colours_ended = True
           if copying:
               new_file.write(line)
           if "ʕっ•ᴥ•ʔっ COLOURS START" in line:
               new_file.write(output)
               copying = False
    
    # DANGER - COULD TRASH YOUR CONFIG HERE!!!!!!
    # test if 'colours_ended' before copying file, cos if second marker not
    # found then something has probably gone wrong
    if colours_ended:
        os.remove(config_file)
        os.rename(tmp_file, config_file)
        print(name + " updated")
    else:
        os.remove(tmp_file)
        print("Couldn't find 'COLOURS END', did not modify " + name)


###############################################################################
## IMPORT COLOURS
###############################################################################

colours = {}
for line in open(schemefile):
    line = ''.join(line.split()) # removes whitespace, space, tab etc.
    if len(line) > 1 and line[0]=="*":
        c = line.lower().split(':')
        name = c[0].lstrip("*.").rstrip(":")
        colours[name] = c[1]

foreground =        colours["foreground"]
foreground_bright = colours["color14"]
highlight1 =        colours["color5"]
highlight1_bright = colours["color13"]
highlight2 =        colours["color4"]
highlight2_bright = colours["color12"]
background =        colours["color8"]
background_bright = colours["color0"]
background_Vbright = colours["color10"]
urgent =            colours["color1"]
urgent_bright =     colours["color9"]

# standard colours
black = "#000000"
white = "#ffffff"

xmonadColours = {
    "cText":                       foreground,
    "cActive":                     highlight1,
    "cBackground":                 background,
    "cVisible":                    background_bright,
    "cDeselected":                 background_bright,
    "cVisibleWorkspaceText":       foreground_bright,
    "cVisibleWorkspaceBackground": background_bright,
    "cUrgent":                     urgent,
    "cActiveTabText":              background_bright,
    "cPrompt":                     background_bright,
    "cPromptHighlight":            highlight2,
    "cHotPrompt":                  urgent,
    "cHotPromptText":              background
}

rofiColours = {
    "normal-foreground":           foreground,
    "normal-background":           background,
    "normal-background-alternate": background_bright,

    "normal-selected-foreground":  background,
    "normal-selected-background":  highlight2,

    "active-foreground":           background,
    "active-background":           highlight1,
    "active-background-alternate": highlight1_bright,

    "urgent-foreground":           background,
    "urgent-background":           urgent,
    "urgent-background-alternate": urgent_bright,

    "border-color":                highlight2
}

dunstColours = {
    "background":     background,
    "foreground":     foreground,
    "frame_low":      highlight2,
    "frame_normal":   highlight2,
    "frame_critical": urgent
}

###############################################################################
## Dunst
###############################################################################

dunst_output  = '\n# %s/\n' % schemefile

dunst_output  += '# shame we had to include the timeouts in here :(\n\n'

for urgency_level in ["low", "normal", "critical"]:

    timeout = "0" if urgency_level == "critical" else "10"

    dunst_output += '[urgency_%s]\n' % urgency_level
    dunst_output += '    frame_color = "%s"\n' % dunstColours["frame_" + urgency_level]
    dunst_output += '    background = "%s"\n' % dunstColours["background"]
    dunst_output += '    foreground = "%s"\n' % dunstColours["foreground"]
    dunst_output += '    timeout = "%s"\n' % timeout
    dunst_output += '\n'

update_config( path = home + ".config/dunst/",
               name = "dunstrc",
               output = dunst_output )

###############################################################################
## Rofi
###############################################################################

rofi_output  = '\n/* %s */\n\n' % schemefile

for name, colour in rofiColours.items():
    rofi_output += '{name}: {colour};\n'.format(name=name, colour=colour) 

rofi_output += "\n"

update_config( path = home + ".config/rofi/",
               name = "config.rasi",
               output = rofi_output )

###############################################################################
## XMonad
###############################################################################

xmonad_output  = '\n-- %s\n\n' % schemefile

for name, colour in xmonadColours.items():
    xmonad_output += '{name} = "{colour}"\n'.format(name=name, colour=colour) 

xmonad_output += "\n"

update_config( path = home + ".xmonad/",
               name = "xmonad.hs",
               output = xmonad_output )

###############################################################################
## XResources
###############################################################################

#copyfile(schemefile, path + 'xresources-current-scheme')

xresources_output = '#include "%s"\n' % schemefile

update_config( path = home,
               name = ".Xresources",
               output = xresources_output )

###############################################################################
## whats-playing
###############################################################################

whats_playing_output = 'colour=%s\n' % colours["color1"]

update_config( path = home + ".scripts/",
               name = "whats-playing",
               output = whats_playing_output )

subprocess.call(["chmod", "+x", home + ".scripts/whats-playing"])

###############################################################################
## Xmobar
###############################################################################

# this one is more complicated because the xmobar syntax is limited, and we
# can't just paste in a bunch of variable definitions.
# so instead we read through a template file and replace, for instance, "+red+"
# with the appropriate hex value.

path = home + ".xmonad/"
name = "xmobar.conf"
template_name = "xmobar-template.hs"

config_file = path + name
tmp_file = config_file + "-tmp"
template_file = path + template_name

xmobar_header_output  = "--\n"
xmobar_header_output += "-- DO NOT EDIT THIS FILE DIRECTLY\n"
xmobar_header_output += "-- To make changes, edit %s\n" % template_file
xmobar_header_output += "-- then run colour-manager.py\n"
xmobar_header_output += "--\n"

copying = True
header_ended = False
with open(template_file) as old_file, open(tmp_file, "w") as new_file:
            for line in old_file:
                    if not copying and "ʕっ•ᴥ•ʔっ HEADER END" in line:
                            copying = True
                            header_ended = True
                    if copying:
                            new_file.write(line)
                    if "ʕっ•ᴥ•ʔっ HEADER START" in line:
                            new_file.write(xmobar_header_output)
                            copying = False

# DANGER - COULD TRASH YOUR CONFIG HERE!!!!!!
# test if 'colours_ended' before copying file, should prove that the both
# markers were found
if header_ended:
    with open(tmp_file, 'r') as new_file:
        config_data = new_file.read()
        
    config_data = config_data.replace('+black+',          colours["color0"])
    config_data = config_data.replace('+black_bright+',   colours["color8"])
    config_data = config_data.replace('+red+',            colours["color1"])
    config_data = config_data.replace('+red_bright+',     colours["color1"])
    config_data = config_data.replace('+green+',          colours["color2"])
    config_data = config_data.replace('+green_bright+',   colours["color10"])
    config_data = config_data.replace('+yellow+',         colours["color3"])
    config_data = config_data.replace('+yellow_bright+',  colours["color11"])
    config_data = config_data.replace('+blue+',           colours["color4"])
    config_data = config_data.replace('+blue_bright+',    colours["color12"])
    config_data = config_data.replace('+magenta+',        colours["color5"])
    config_data = config_data.replace('+magenta_bright+', colours["color13"])
    config_data = config_data.replace('+cyan+',           colours["color6"])
    config_data = config_data.replace('+cyan_bright+',    colours["color14"])
    config_data = config_data.replace('+white+',          colours["color7"])
    config_data = config_data.replace('+white_bright+',   colours["color15"])
    # config_data = config_data.replace('+background+',     black)
    config_data = config_data.replace('+background+',     colours["color8"])
    config_data = config_data.replace('+foreground+',     colours["foreground"])


    with open(tmp_file, 'w') as new_file:
        new_file.write(config_data)

    os.remove(config_file)
    os.rename(tmp_file, config_file)
    print(name + " updated")
else:
    os.remove(tmp_file)
    print("Couldn't find 'HEADER END', did not modify " + name)

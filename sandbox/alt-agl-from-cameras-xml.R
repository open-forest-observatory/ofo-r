# NOTE: This is a work in progress for getting lat/lon/alt from a metashape camera position XML and
# using it to get the height of the estimated cameras above ground.

library(tidyverse)
library(elevatr)
library(xml2)

d = read_xml("/ofo-share/str-disp_drone-data-partial/imagery-processed/outputs/120m-01/Lassic-120m_20240213T0503_cameras.xml")
cams = xml_find_all(d, ".//camera")
l = as_list(cams)

cam = l[[850]]

label = attributes(cam)$label

ref = cam$reference
x = attributes(ref)$x
y = attributes(ref)$y
z = attributes(ref)$z

cam_pos = data.frame(label = label,
                     x = x,
                     y = y,
                     z = z)
cam_pos

x
y
z


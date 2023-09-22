
function [rgbimg]=grey2rgb(img, w_red, w_green, w_blue)
    rgbimg = cat(3, img.*w_red, img.*w_green, img.*w_blue);
    
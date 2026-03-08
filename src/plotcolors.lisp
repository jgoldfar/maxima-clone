;; Functions to manage color in plot2/plot3d and draw2d/draw3d
(in-package :maxima)

;; Converts a string to lower-case, removing spaces, minus signs, and underscores
(defun atom-to-downcased-string (val)
   (remove-if
      #'(lambda (z) (member (char-int z) '(32 45 95)))
      (string-downcase
        (string-trim 
           "\""
           (coerce (mstring val) 'string)))))

(defvar *color-table* (make-hash-table :test 'equal))
(setf (gethash "aliceblue" *color-table*) "#f0f8ff"
      (gethash "antiquewhite" *color-table*) "#faebd7"
      (gethash "aqua" *color-table*) "#00ffff"
      (gethash "aquamarine" *color-table*) "#7fffd4"
      (gethash "azure" *color-table*) "#f0ffff"
      (gethash "beige" *color-table*) "#f5f5dc"
      (gethash "bisque" *color-table*) "#ffe4c4"
      (gethash "black" *color-table*) "#000000"
      (gethash "blanchedalmond" *color-table*) "#ffebcd"
      (gethash "blue" *color-table*) "#0000ff"
      (gethash "blueviolet" *color-table*) "#8a2be2"
      (gethash "brown" *color-table*) "#a52a2a"
      (gethash "burlywood" *color-table*) "#deb887"
      (gethash "cadetblue" *color-table*) "#5f9ea0"
      (gethash "chocolate" *color-table*) "#d2691e"
      (gethash "coral" *color-table*) "#ff7f50"
      (gethash "cornflowerblue" *color-table*) "#6495ed"
      (gethash "cornsilk" *color-table*) "#fff8dc"
      (gethash "crimson" *color-table*) "#dc143c"
      (gethash "cyan" *color-table*) "#00ffff"
      (gethash "darkblue" *color-table*) "#00008b"
      (gethash "darkcyan" *color-table*) "#008b8b"
      (gethash "darkgoldenrod" *color-table*) "#b8860b"
      (gethash "darkgray" *color-table*) "#a9a9a9"
      (gethash "darkgrey" *color-table*) "#a9a9a9"
      (gethash "darkgreen" *color-table*) "#006400"
      (gethash "darkkhaki" *color-table*) "#bdb76b"
      (gethash "darkmagenta" *color-table*) "#8b008b"
      (gethash "darkolivegreen" *color-table*) "#556b2f"
      (gethash "darkorange" *color-table*) "#ff8c00"
      (gethash "darkorchid" *color-table*) "#9932cc"
      (gethash "darkred" *color-table*) "#8b0000"
      (gethash "darksalmon" *color-table*) "#e9967a"
      (gethash "darkseagreen" *color-table*) "#8fbc8f"
      (gethash "darkslateblue" *color-table*) "#483d8b"
      (gethash "darkslategray" *color-table*) "#2f4f4f"
      (gethash "darkslategrey" *color-table*) "#2f4f4f"
      (gethash "darkturquoise" *color-table*) "#00ced1"
      (gethash "darkviolet" *color-table*) "#9400d3"
      (gethash "deeppink" *color-table*) "#ff1493"
      (gethash "deepskyblue" *color-table*) "#00bfff"
      (gethash "dimgray" *color-table*) "#696969"
      (gethash "dimgrey" *color-table*) "#696969"
      (gethash "dodgerblue" *color-table*) "#1e90ff"
      (gethash "firebrick" *color-table*) "#b22222"
      (gethash "floralwhite" *color-table*) "#fffaf0"
      (gethash "forestgreen" *color-table*) "#228b22"
      (gethash "fuchsia" *color-table*) "#ff00ff"
      (gethash "gainsboro" *color-table*) "#dcdcdc"
      (gethash "ghostwhite" *color-table*) "#f8f8ff"
      (gethash "gold" *color-table*) "#ffd700"
      (gethash "goldenrod" *color-table*) "#daa520"
      (gethash "gray" *color-table*) "#bebebe"
      (gethash "grey" *color-table*) "#bebebe"
      (gethash "green" *color-table*) "#00ff00" 
      (gethash "greenyellow" *color-table*) "#adff2f"
      (gethash "honeydew" *color-table*) "#f0fff0"
      (gethash "hotpink" *color-table*) "#ff69b4"
      (gethash "indianred" *color-table*) "#cd5c5c"
      (gethash "indigo" *color-table*) "#4b0082"
      (gethash "ivory" *color-table*) "#fffff0"
      (gethash "khaki" *color-table*) "#f0e68c"
      (gethash "lavenderblush" *color-table*) "#fff0f5"
      (gethash "lavender" *color-table*) "#e6e6fa"
      (gethash "lawngreen" *color-table*) "#7cfc00"
      (gethash "lemonchiffon" *color-table*) "#fffacd"
      (gethash "lightblue" *color-table*) "#add8e6"
      (gethash "lightcoral" *color-table*) "#f08080"
      (gethash "lightcyan" *color-table*) "#e0ffff"
      (gethash "lightgoldenrodyellow" *color-table*) "#fafad2"
      (gethash "lightgray" *color-table*) "#d3d3d3"
      (gethash "lightgrey" *color-table*) "#d3d3d3"
      (gethash "lightgreen" *color-table*) "#90ee90"
      (gethash "lightpink" *color-table*) "#ffb6c1"
      (gethash "lightsalmon" *color-table*) "#ffa07a"
      (gethash "lightseagreen" *color-table*) "#20b2aa"
      (gethash "lightskyblue" *color-table*) "#87cefa"
      (gethash "lightslateblue" *color-table*) "#8470ff"
      (gethash "lightslategray" *color-table*) "#778899"
      (gethash "lightslategrey" *color-table*) "#778899"
      (gethash "lightsteelblue" *color-table*) "#b0c4de"
      (gethash "lightyellow" *color-table*) "#ffffe0"
      (gethash "lime" *color-table*) "#00ff00"
      (gethash "limegreen" *color-table*) "#32cd32"
      (gethash "linen" *color-table*) "#faf0e6"
      (gethash "magenta" *color-table*) "#ff00ff"
      (gethash "maroon" *color-table*) "#b03060"
      (gethash "mediumaquamarine" *color-table*) "#66cdaa"
      (gethash "mediumblue" *color-table*) "#0000cd"
      (gethash "mediumorchid" *color-table*) "#ba55d3"
      (gethash "mediumpurple" *color-table*) "#9370db"
      (gethash "mediumseagreen" *color-table*) "#3cb371"
      (gethash "mediumslateblue" *color-table*) "#7b68ee"
      (gethash "mediumspringgreen" *color-table*) "#00fa9a"
      (gethash "mediumturquoise" *color-table*) "#48d1cc"
      (gethash "mediumvioletred" *color-table*) "#c71585"
      (gethash "midnightblue" *color-table*) "#191970"
      (gethash "mintcream" *color-table*) "#f5fffa"
      (gethash "mistyrose" *color-table*) "#ffe4e1"
      (gethash "moccasin" *color-table*) "#ffe4b5"
      (gethash "navajowhite" *color-table*) "#ffdead"
      (gethash "navy" *color-table*) "#000080"
      (gethash "oldlace" *color-table*) "#fdf5e6"
      (gethash "olive" *color-table*) "#808000"
      (gethash "olivedrab" *color-table*) "#6b8e23"
      (gethash "orange" *color-table*) "#ffa500"
      (gethash "orangered" *color-table*) "#ff4500"
      (gethash "orchid" *color-table*) "#da70d6"
      (gethash "palegoldenrod" *color-table*) "#eee8aa"
      (gethash "palegreen" *color-table*) "#98fb98"
      (gethash "paleturquoise" *color-table*) "#afeeee"
      (gethash "palevioletred" *color-table*) "#db7093"
      (gethash "papayawhip" *color-table*) "#ffefd5"
      (gethash "peachpuff" *color-table*) "#ffdab9"
      (gethash "peru" *color-table*) "#cd853f"
      (gethash "pink" *color-table*) "#ffc0cb"
      (gethash "plum" *color-table*) "#dda0dd"
      (gethash "powderblue" *color-table*) "#b0e0e6"
      (gethash "purple" *color-table*) "#a020f0"
      (gethash "rebeccapurple" *color-table*) "#663399"
      (gethash "red" *color-table*) "#ff0000"
      (gethash "rosybrown" *color-table*) "#bc8f8f"
      (gethash "royalblue" *color-table*) "#4169e1"
      (gethash "saddlebrown" *color-table*) "#8b4513"
      (gethash "salmon" *color-table*) "#fa8072"
      (gethash "sandybrown" *color-table*) "#f4a460"
      (gethash "seagreen" *color-table*) "#2e8b57"
      (gethash "seashell" *color-table*) "#fff5ee"
      (gethash "sienna" *color-table*) "#a0522d"
      (gethash "silver" *color-table*) "#c0c0c0"
      (gethash "skyblue" *color-table*) "#87ceeb"
      (gethash "slateblue" *color-table*) "#6a5acd"
      (gethash "slategray" *color-table*) "#708090"
      (gethash "slategrey" *color-table*) "#708090"
      (gethash "snow" *color-table*) "#fffafa"
      (gethash "springgreen" *color-table*) "#00ff7f"
      (gethash "steelblue" *color-table*) "#4682b4"
      (gethash "tan" *color-table*) "#d2b48c"
      (gethash "teal" *color-table*) "#008080"
      (gethash "thistle" *color-table*) "#d8bfd8"
      (gethash "tomato" *color-table*) "#ff6347"
      (gethash "turquoise" *color-table*) "#40e0d0" 
      (gethash "violet" *color-table*) "#ee82ee"
      (gethash "wheat" *color-table*) "#f5deb3"
      (gethash "white" *color-table*) "#ffffff"
      (gethash "whitesmoke" *color-table*) "#f5f5f5"
      (gethash "yellow" *color-table*) "#ffff00"
      (gethash "yellowgreen" *color-table*) "#9acd32")

;; Returns true if the given symbol or string is a valid plot color;
;; namely, a valid color name or a six-digit hexadecimal number with a # suffix.
;; The color names can be in upper or lower case (or a mix of both)
;; and with any spaces, hyphens or undescores.
(defun plotcolorp (clr)
  (let ((color (atom-to-downcased-string (ensure-string clr))))
    (cond ((and (stringp color) (string= (subseq color 0 1) "#")
                (= (length color) 7)
                (ignore-errors (parse-integer (subseq color 1 6) :radix 16)))
           t)
          ((some #'(lambda (z) (string= z color))
                 (loop for k being the hash-keys of *color-table* collect k))
           t)
          (t nil))))

;; given a color in the #rrggbb notation, returns a list with red, green
;; and blue values between 0.0 and 1.0
(defun hex-to-numeric-list (str)
  (let ((hex1 (subseq str 1 3))
        (hex2 (subseq str 3 5))
        (hex3 (subseq str 5 7)))
    (list
     (/ (parse-integer hex1 :radix 16) 255.0)
     (/ (parse-integer hex2 :radix 16) 255.0)
     (/ (parse-integer hex3 :radix 16) 255.0))))

;; Returns the corresponding color code forthe given symbol or string, in the
;; for ##rrggbb, where rr, gg and bb are hexadecimal numbers beween 0 and 255,
;; or nil if the symbol or string does not represent a valid color.
(defun rgb-color (clr)
  (let ((color (atom-to-downcased-string (ensure-string clr))) code)
    (cond ((and (stringp color) (string= (subseq color 0 1) "#")
             (= (length color) 7)
             (ignore-errors (parse-integer (subseq color 1 6) :radix 16)))
           color)
          ((setf code (gethash color *color-table*)) code)
          (t nil))))
        

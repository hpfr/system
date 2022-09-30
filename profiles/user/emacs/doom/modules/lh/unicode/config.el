;;; config.el --- Description -*- lexical-binding: t; -*-

(setq doom-symbol-fallback-font-families nil
      doom-emoji-fallback-font-families nil
      emojify-display-style 'unicode)

;; ;; missing:
;; (vithkuqi rumi-number old-uyghur chorasmian
;; sinhala-archaic-number dives-akuru nandinagari makasar
;; cypro-minoan tangsa khitan-small-script znamenny-musical-notation
;; counting-rod-numeral toto ottoman-siyaq-number)

;; FIXME: performance. pcase?
(let* ((iosevka-scripts '(latin greek phonetic coptic cyrillic symbol mathematical))
       (source-scripts '(latin greek phonetic coptic cyrillic symbol))

       (cjk-scripts '(cjk-misc han kana hangul
                               ideographic-description vertical-form
                               bopomofo kanbun symbol mathematical))

       (devanagari-scripts '(devanagari vedic north-indic-number))

       (noto-serif-scripts '(ahom dogra nyiakeng-puachue-hmong yezidi))

       (noto-sans-scripts
        '(adlam arabic avestan bamum bassa-vah batak bhaiksuki brahmi
                buginese buhid canadian-aboriginal carian
                caucasian-albanian chakma cham cherokee coptic
                cuneiform cuneiform-numbers-and-punctuation deseret
                elbasan elymaic glagolitic gothic gunjala-gondi
                hanifi-rohingya hanunoo hatran inscriptional-pahlavi
                inscriptional-parthian javanese kaithi kayah-li
                kharoshthi khudawadi lepcha limbu linear-a lisu lycian
                lydian mahajani mandaic manichaean marchen
                masaram-gondi medefaidrin mende-kikakui meroitic miao
                modi mongolian mro multani nko nabataean newa nushu
                ogham ol-chiki old-hungarian old-italic
                old-north-arabian old-sogdian old-permic old-persian
                old-south-arabian old-turkic oriya osage osmanya
                pahawh-hmong palmyrene pau-cin-hau phoenician
                psalter-pahlavi rejang runic samaritan saurashtra
                sharada shavian siddham sogdian sora-sompeng soyombo
                sundanese syloti-nagri syriac tagalog tagbanwa tai-le
                tai-tham tai-viet takri thaana tifinagh tirhuta
                ugaritic vai wancho warang-citi yi zanabazar-square))

       (noto-both-scripts
        '(armenian balinese bengali devanagari ethiopic georgian
                   grantha gujarati gurmukhi hebrew kannada khmer khojki
                   lao malayalam sinhala tamil telugu thai tibetan))

       (noto-script-to-font
        (lambda (script &optional serif)
                    (list (concat "Noto " (if serif "Serif" "Sans") " "
                                  (capitalize (string-replace "-" " " (format "%s" script)))) script)))
       (noto-script-to-sans-font (lambda (script) (funcall noto-script-to-font script)))
       (noto-script-to-serif-font (lambda (script) (funcall noto-script-to-font script t)))
       (noto-sans-font-scripts (append (mapcar noto-script-to-sans-font noto-sans-scripts)
                                       (mapcar noto-script-to-sans-font noto-both-scripts)))
       (noto-serif-font-scripts (append (mapcar noto-script-to-serif-font noto-serif-scripts)
                                        (mapcar noto-script-to-serif-font noto-both-scripts)))
       ;; https://notofonts.github.io/noto-docs/specimen/
       (font-scripts `(("monospace" ,@iosevka-scripts)
                       ("Iosevka Term" ,@iosevka-scripts)
                       ("Iosevka Term Curly Slab" ,@iosevka-scripts)
                       ("sans-serif" ,@source-scripts)
                       ("Source Sans 3 VF" ,@source-scripts)
                       ;; colon ensures string is parsed as fontconfig name, not GTK pattern with size
                       ("Source Sans 3:" ,@source-scripts)
                       ("Source Serif 4 Variable" ,@source-scripts)
                       ("Source Serif 4:" ,@source-scripts)
                       ("Source Han Mono" ,@cjk-scripts)
                       ("Source Han Sans" ,@cjk-scripts)
                       ("Source Han Serif" ,@cjk-scripts)
                       ("JuliaMono" symbol mathematical)
                       ("STIX Two Math" mathematical)
                       ("Noto Emoji" emoji)
                       ("Noto Color Emoji" emoji)

                       ("Noto Music" byzantine-musical-symbol musical-symbol ancient-greek-musical-notation symbol)
                       ("Noto Sans Symbols2" ancient-greek-number ancient-symbol braille phaistos-disc tai-xuan-jing-symbol mathematical
                        mahjong-tile domino-tile playing-cards chess-symbol symbol)
                       ("Noto Sans Devanagari" ,@devanagari-scripts)
                       ("Noto Serif Devanagari" ,@devanagari-scripts)
                       ("Noto Sans Linear B" linear-b aegean-number)

                       ("Noto Kufi Arabic" arabic)
                       ("Noto Looped Lao" lao)
                       ("Noto Looped Thai" thai)
                       ("Noto Naskh Arabic" arabic)
                       ("Noto Nastaliq Urdu" arabic)
                       ("Noto Rashi Hebrew" hebrew)
                       ("Noto Sans Adlam Unjoined" adlam)
                       ("Noto Sans Anatolian Hieroglyphs" anatolian)
                       ("Noto Sans Cypriot" cypriot-syllabary)
                       ("Noto Sans Duployan" duployan-shorthand)
                       ("Noto Sans Egyptian Hieroglyphs" egyptian)
                       ("Noto Sans Imperial Aramaic" aramaic)
                       ("Noto Sans Indic Siyaq Numbers" indic-siyaq-number)
                       ("Noto Sans Math" mathematical)
                       ("Noto Sans Mayan Numerals" mayan-numeral)
                       ("Noto Sans MeeteiMayek" meetei-mayek)
                       ("Noto Sans Myanmar" burmese)
                       ("Noto Sans New Tai Lue" tai-lue)
                       ("Noto Sans PhagsPa" phags-pa)
                       ("Noto Sans SignWriting" sutton-sign-writing)
                       ("Noto Sans Tamil Supplement" tamil)
                       ("Noto Sans Tifinagh APT" tifinagh)
                       ("Noto Sans Tifinagh Adrar" tifinagh)
                       ("Noto Sans Tifinagh Agraw Imazighen" tifinagh)
                       ("Noto Sans Tifinagh Ahaggar" tifinagh)
                       ("Noto Sans Tifinagh Air" tifinagh)
                       ("Noto Sans Tifinagh Azawagh" tifinagh)
                       ("Noto Sans Tifinagh Ghat" tifinagh)
                       ("Noto Sans Tifinagh Hawad" tifinagh)
                       ("Noto Sans Tifinagh Rhissa Ixa" tifinagh)
                       ("Noto Sans Tifinagh SIL" tifinagh)
                       ("Noto Sans Tifinagh Tawellemmet" tifinagh)
                       ("Noto Serif Myanmar" burmese)
                       ("Noto Serif Tangut" tangut tangut-components)
                       ("Noto Tranditional Nushu" nushu)

                       ,@noto-sans-font-scripts
                       ,@noto-serif-font-scripts))


       (shared-monospace '("Source Han Mono" "JuliaMono" "Noto Emoji"))
       (noto-sans-fonts (append
                         '("Noto Sans Symbols2" "Noto Sans Linear B" "Noto Sans Devanagari" "Noto Music"
                           "Noto Sans Anatolian Hieroglyphs" "Noto Sans Cypriot"
                           "Noto Sans Duployan" "Noto Sans Egyptian Hieroglyphs"
                           "Noto Sans Imperial Aramaic" "Noto Sans Indic Siyaq Numbers"
                           "Noto Sans Math" "Noto Sans Mayan Numerals"
                           "Noto Sans MeeteiMayek" "Noto Sans Myanmar"
                           "Noto Sans New Tai Lue" "Noto Sans PhagsPa"
                           "Noto Sans SignWriting" "Noto Sans Tamil Supplement"
                           "Noto Sans Tifinagh APT" "Noto Sans Tifinagh Adrar"
                           "Noto Sans Tifinagh Agraw Imazighen" "Noto Sans Tifinagh Ahaggar"
                           "Noto Sans Tifinagh Air" "Noto Sans Tifinagh Azawagh"
                           "Noto Sans Tifinagh Ghat" "Noto Sans Tifinagh Hawad"
                           "Noto Sans Tifinagh Rhissa Ixa" "Noto Sans Tifinagh SIL"
                           "Noto Sans Tifinagh Tawellemmet")
                         (mapcar #'car noto-sans-font-scripts)))
       (noto-serif-fonts (append
                          '("Noto Serif Devanagari" "Noto Serif Myanmar" "Noto Serif Tangut")
                          (mapcar #'car noto-serif-font-scripts)))
       (make-fontset
        (lambda (fonts)
          (mapcan (lambda (font)
                    (mapcar (lambda (script) (list script font))
                            (alist-get font font-scripts nil nil #'string=)))
                  fonts)))
       ;; NOTE: fallback to other fontsets in fontsets doesn't work, splice
       ;; variable with contents
       ;; NOTE: variable fonts don't play well with attributes like weight, use standard
       (fontset-sans-serif (funcall make-fontset `("Source Sans 3:"
                                                   "Source Han Sans"
                                                   "Noto Color Emoji"
                                                   ,@noto-sans-fonts
                                                   ,@noto-serif-fonts)))
       (fontset-serif (append (funcall make-fontset `("Source Serif 4:"
                                                      "Source Han Serif"
                                                      "Noto Color Emoji"
                                                      "STIX Two Math"
                                                      ,@noto-serif-fonts))
                              fontset-sans-serif))
       (fontset-monospace (append (funcall make-fontset `("monospace"
                                                          ,@shared-monospace))
                                  fontset-sans-serif))
       (fontset-monospace-serif (append (funcall make-fontset `("Iosevka Term Curly Slab"
                                                                ,@shared-monospace))
                                        fontset-serif)))

  (new-fontset "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-sans-serif" fontset-sans-serif)
  (new-fontset "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-serif" fontset-serif)
  (new-fontset "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-monospace" fontset-monospace)
  (new-fontset "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-monospace-serif" fontset-monospace-serif))

;; emacs doesn't have this by default for some reason
(defface variable-pitch-serif
  '((t (:family "fontset-serif")))
  "A variable-pitch face with serifs."
  :group 'basic-faces)

;; 12pt
(set-face-attribute 'default nil :height 120 :font "fontset-monospace" :fontset "fontset-monospace")

;; make every face relative to default, so we can resize easily
(set-face-attribute 'fixed-pitch nil :height 1.0 :font "fontset-monospace" :fontset "fontset-monospace")
(set-face-attribute 'fixed-pitch-serif nil :height 1.0 :font "fontset-monospace-serif" :fontset "fontset-monospace-serif")
(set-face-attribute 'variable-pitch nil :height 1.0 :font "fontset-sans-serif" :fontset "fontset-sans-serif")
(set-face-attribute 'variable-pitch-serif nil :height 1.0 :font "fontset-serif" :fontset "fontset-serif")

// Copyright 2022 Framework Computer
// SPDX-License-Identifier: GPL-2.0-or-later
//
// Custom Dvorak keymap for Framework 16 ANSI keyboard

#include QMK_KEYBOARD_H
#include "framework.h"

enum _layers {
    _DVORAK,
    _FN,
    _FN_LOCK,
    _FM,
    _QWERTY
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [_DVORAK] = LAYOUT(
        KC_ESC,  KC_MUTE, KC_VOLD, KC_VOLU, KC_MPRV, KC_MPLY, KC_MNXT, KC_BRID, KC_BRIU, KC_SCRN, KC_AIRP, KC_PSCR, KC_MSEL, KC_DEL,
        KC_GRV,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_LBRC, KC_RBRC, KC_BSPC,
        KC_TAB,  KC_QUOT, KC_COMM, KC_DOT,  KC_P,    KC_Y,    KC_F,    KC_G,    KC_C,    KC_R,    KC_L,    KC_SLSH, KC_EQL,  KC_BSLS,
        KC_LCTL, KC_A,    KC_O,    KC_E,    KC_U,    KC_I,    KC_D,    KC_H,    KC_T,    KC_N,    KC_S,    KC_MINS,          KC_ENT,
        KC_LSFT,          KC_SCLN, KC_Q,    KC_J,    KC_K,    KC_X,    KC_B,    KC_M,    KC_W,    KC_V,    KC_Z,             KC_RSFT,
        KC_LCTL, MO(_FN), KC_LGUI, KC_LALT,          KC_SPC,                    KC_RALT, KC_RCTL, KC_LEFT, KC_UP,   KC_DOWN, KC_RGHT
    ),
    [_FN] = LAYOUT(
        FN_LOCK, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  KC_F11,  KC_F12,  KC_INS,
        _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
        TG(_QWERTY), _______, RGB_TOG, RGB_MOD, RGB_HUI, RGB_SAI, RGB_SPI, RGB_VAI, _______, _______, KC_PAUS, _______, _______, QK_BOOT,
        KC_CAPS, _______, _______, RGB_RMOD,RGB_HUD, RGB_SAD, RGB_SPD, RGB_VAD, KC_SCRL, _______, _______, _______,          _______,
        _______,          _______, _______, BL_BRTG, _______, KC_BRK,  _______, _______, _______, _______, _______,          _______,
        _______, _______, _______, _______,          BL_STEP,                   _______, _______, KC_HOME, KC_PGUP, KC_PGDN, KC_END
    ),
    [_FN_LOCK] = LAYOUT(
        _______, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  KC_F11,  KC_F12,  _______,
        _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
        _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
        _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,          _______,
        _______,          _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,          _______,
        _______, MO(_FM), _______, _______,          _______,                   _______, _______, _______, _______, _______, _______
    ),
    [_FM] = LAYOUT(
        FN_LOCK, KC_MUTE, KC_VOLD, KC_VOLU, KC_MPRV, KC_MPLY, KC_MNXT, KC_BRID, KC_BRIU, KC_SCRN, KC_AIRP, KC_PSCR, KC_MSEL, KC_INS,
        _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
        _______, _______, RGB_TOG, RGB_MOD, RGB_HUI, RGB_SAI, RGB_SPI, RGB_VAI, _______, _______, KC_PAUS, _______, _______, _______,
        _______, _______, _______, RGB_RMOD,RGB_HUD, RGB_SAD, RGB_SPD, RGB_VAD, KC_SCRL, _______, _______, _______,          _______,
        _______,          _______, _______, BL_BRTG, _______, KC_BRK,  _______, _______, _______, _______, _______,          _______,
        _______, _______, _______, _______,          BL_STEP,                   _______, _______, KC_HOME, KC_PGUP, KC_PGDN, KC_END
    ),
    // QWERTY overlay - toggle with FN+Q. Overrides alpha and punctuation keys.
    [_QWERTY] = LAYOUT(
        _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
        _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
        _______, KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC, KC_RBRC, _______,
        _______, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,          _______,
        _______,          KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH,          _______,
        _______, _______, _______, _______,          _______,                   _______, _______, _______, _______, _______, _______
    ),
};

// === Per-key RGB indicators ===

// LED indices (from ansi.c Key Matrix to LED Index)
// Home row index fingers
#define LED_U_F    43  // K34: U (Dvorak) / F (QWERTY)
#define LED_H_J    49  // K37: H (Dvorak) / J (QWERTY)
// Modifiers
#define LED_CAPS   44  // K30: Caps Lock position (remapped to LCtrl)
#define LED_LSFT   32  // K44: Left Shift
#define LED_RSFT   92  // K57: Right Shift
#define LED_LCTL   34  // K58: Left Ctrl
#define LED_FN     93  // K59: FN key
#define LED_GUI    46  // K127: GUI/Super
#define LED_LALT   47  // K60: Left Alt
#define LED_RALT   84  // K62: Right Alt
#define LED_RCTL   88  // K64: Right Ctrl
// Top row (function row)
#define LED_ESC    25  // K00
#define LED_MUTE   21  // K01
#define LED_VOLD   19  // K02
#define LED_VOLU   18  // K03
#define LED_MPRV   20  // K04
#define LED_MPLY   22  // K05
#define LED_MNXT   24  // K06
#define LED_BRID   26  // K07
#define LED_BRIU   67  // K08
#define LED_SCRN   74  // K09
#define LED_AIRP   68  // K0A
#define LED_PSCR   66  // K0B
#define LED_MSEL   70  // K0C
#define LED_DEL    73  // K0D
// FN layer specials
#define LED_TG_QW   1  // K16 (Tab): TG(QWERTY) on FN layer
#define LED_BOOT   55  // K29: QK_BOOT on FN layer

// Colors                    R     G     B
#define CLR_CYAN       0,  180,  180
#define CLR_ORANGE   255,  120,    0
#define CLR_PURPLE   128,    0,  255
#define CLR_BLUE       0,   80,  255
#define CLR_GREEN      0,  180,    0
#define CLR_AMBER    255,  160,    0
#define CLR_DIM_WHT  100,  100,  100
#define CLR_YELLOW   255,  255,    0
#define CLR_RED      255,    0,    0
// Dim colors for top row
#define CLR_DIM_TEAL   0,   25,   20
#define CLR_DIM_PINK  25,    0,   15
#define CLR_DIM_GOLD  25,   21,    0
#define CLR_DIM_SLATE 13,   14,   18
#define CLR_DIM_MELON 50,  25,   18

// Scale an RGB color by the current brightness (0-255)
static inline void set_color_scaled(uint8_t index, uint8_t r, uint8_t g, uint8_t b) {
    uint8_t val = rgb_matrix_get_val();
    rgb_matrix_set_color(index,
        (uint8_t)((uint16_t)r * val / 255),
        (uint8_t)((uint16_t)g * val / 255),
        (uint8_t)((uint16_t)b * val / 255));
}

bool rgb_matrix_indicators_advanced_user(uint8_t led_min, uint8_t led_max) {
    // Turn off all LEDs first, we only light the keys we care about
    for (uint8_t i = led_min; i < led_max; i++) {
        rgb_matrix_set_color(i, 0, 0, 0);
    }

    // --- Modifier keys (always shown) ---
    set_color_scaled(LED_LCTL, CLR_BLUE);
    set_color_scaled(LED_RCTL, CLR_BLUE);
    set_color_scaled(LED_LSFT, CLR_GREEN);
    set_color_scaled(LED_RSFT, CLR_GREEN);
    set_color_scaled(LED_LALT, CLR_AMBER);
    set_color_scaled(LED_RALT, CLR_AMBER);
    set_color_scaled(LED_GUI,  CLR_DIM_WHT);
    set_color_scaled(LED_FN,   CLR_PURPLE);

    // --- Caps/Ctrl key: blue normally, green on FN layer (caps lock = shift color) ---
    if (layer_state_is(_FN) || layer_state_is(_FM)) {
        set_color_scaled(LED_CAPS, CLR_GREEN);
    } else {
        set_color_scaled(LED_CAPS, CLR_BLUE);
    }

    // --- Home row index fingers: cyan for Dvorak, orange for QWERTY ---
    if (layer_state_is(_QWERTY)) {
        set_color_scaled(LED_U_F, CLR_ORANGE);
        set_color_scaled(LED_H_J, CLR_ORANGE);
    } else {
        set_color_scaled(LED_U_F, CLR_CYAN);
        set_color_scaled(LED_H_J, CLR_CYAN);
    }

    // --- Top row ---
    // F-keys shown when: FN held (normal mode), or FN_LOCK active without FN held
    // Media keys shown when: normal mode without FN, or FN held in FN_LOCK mode (_FM)
    bool fn_lock_default = (get_highest_layer(default_layer_state) == _FN_LOCK);
    bool fkeys_shown = layer_state_is(_FN) || (fn_lock_default && !layer_state_is(_FM));

    if (fkeys_shown) {
        // F-keys active: dim melon for F1-F12
        set_color_scaled(LED_MUTE, CLR_DIM_MELON);  // F1
        set_color_scaled(LED_VOLD, CLR_DIM_MELON);  // F2
        set_color_scaled(LED_VOLU, CLR_DIM_MELON);  // F3
        set_color_scaled(LED_MPRV, CLR_DIM_MELON);  // F4
        set_color_scaled(LED_MPLY, CLR_DIM_MELON);  // F5
        set_color_scaled(LED_MNXT, CLR_DIM_MELON);  // F6
        set_color_scaled(LED_BRID, CLR_DIM_MELON);  // F7
        set_color_scaled(LED_BRIU, CLR_DIM_MELON);  // F8
        set_color_scaled(LED_SCRN, CLR_DIM_MELON);  // F9
        set_color_scaled(LED_AIRP, CLR_DIM_MELON);  // F10
        set_color_scaled(LED_PSCR, CLR_DIM_MELON);  // F11
        set_color_scaled(LED_MSEL, CLR_DIM_MELON);  // F12
    } else {
        // Default layer: color by functional group
        // Volume: dim teal
        set_color_scaled(LED_MUTE, CLR_DIM_TEAL);
        set_color_scaled(LED_VOLD, CLR_DIM_TEAL);
        set_color_scaled(LED_VOLU, CLR_DIM_TEAL);
        // Media playback: dim pink
        set_color_scaled(LED_MPRV, CLR_DIM_PINK);
        set_color_scaled(LED_MPLY, CLR_DIM_PINK);
        set_color_scaled(LED_MNXT, CLR_DIM_PINK);
        // Brightness: dim gold
        set_color_scaled(LED_BRID, CLR_DIM_GOLD);
        set_color_scaled(LED_BRIU, CLR_DIM_GOLD);
        // Screen capture: dim slate
        set_color_scaled(LED_PSCR, CLR_DIM_SLATE);
    }

    // --- FN layer specials ---
    if (layer_state_is(_FN) || layer_state_is(_FM)) {
        set_color_scaled(LED_TG_QW, CLR_YELLOW);
        set_color_scaled(LED_BOOT,  CLR_RED);
    }

    return false;
}

// FN Lock behavior
bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
        case FN_LOCK:
            if (record->event.pressed) {
                if (layer_state_is(_FN)) {
                    set_single_persistent_default_layer(_FN_LOCK);
                }
                if (layer_state_is(_FM)) {
                    set_single_persistent_default_layer(_DVORAK);
                }
            }
            return false;
            break;
        default:
            break;
    }
    return true;
}

// Copyright 2022 Framework Computer
// SPDX-License-Identifier: GPL-2.0-or-later
//
// Custom macropad keymap for Framework 16 RGB Macropad.
//
// Five layers:
//   L0 _NUM_ON  - numpad (matches OS Num Lock = on)
//   L1 _NUM_OFF - numpad-off / navigation (OS Num Lock = off)
//   L2 _HYPR    - 4x4 grid sends HYPR(A..P) = Ctrl+Alt+Shift+Super+letter
//   L3 _LCAG    - 4x4 grid sends LCAG(A..P) = Ctrl+Alt+Super+letter
//   L4 _MEH     - 4x4 grid sends MEH(A..P)  = Ctrl+Alt+Shift+letter
//
// Layout:
//   Top row (H1..H4): layer switches on every layer.
//     H1 = KC_NUM (toggles OS Num Lock; led_update_user moves between L0/L1).
//     H2..H4 = TO(_HYPR/_LCAG/_MEH).
//   Middle 4x4: per-layer content.
//   Bottom row: Home / PgUp / PgDn / End on every layer.

#include QMK_KEYBOARD_H

enum my_layers {
    _NUM_ON  = 0,   // matches _NUMLOCK in macropad.h
    _NUM_OFF = 1,   // matches _FN in macropad.h
    _HYPR,
    _LCAG,
    _MEH,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [_NUM_ON] = LAYOUT(
        KC_NUM,     TO(_HYPR),  TO(_LCAG),  TO(_MEH),
        KC_NUM,     KC_PSLS,    KC_PAST,    KC_EQL,
        KC_P7,      KC_P8,      KC_P9,      KC_PMNS,
        KC_P4,      KC_P5,      KC_P6,      KC_PPLS,
        KC_P1,      KC_P2,      KC_P3,      KC_PENT,
        KC_P0,      KC_P0,      KC_PDOT,    KC_PENT
    ),
    // _NUM_OFF: transparent middle grid falls through to _NUM_ON (the default
    // layer). The OS reinterprets KC_P4..KC_P9 as arrows / Home / End / PgUp /
    // PgDn / Ins / Del while Num Lock is off, matching the default keymap's
    // behavior. RGB/backlight controls take the col-4 slots as in default _FN.
    [_NUM_OFF] = LAYOUT(
        KC_NUM,     TO(_HYPR),  TO(_LCAG),  TO(_MEH),
        _______,    _______,    _______,    _______,
        _______,    _______,    _______,    _______,
        _______,    _______,    _______,    _______,
        _______,    _______,    _______,    _______,
        KC_HOME,    KC_PGUP,    KC_PGDN,    KC_END
    ),
    [_HYPR] = LAYOUT(
        KC_NUM,     TO(_HYPR),  TO(_LCAG),  TO(_MEH),
        HYPR(KC_A), HYPR(KC_B), HYPR(KC_C), HYPR(KC_D),
        HYPR(KC_E), HYPR(KC_F), HYPR(KC_G), HYPR(KC_H),
        HYPR(KC_I), HYPR(KC_J), HYPR(KC_K), HYPR(KC_L),
        HYPR(KC_M), HYPR(KC_N), HYPR(KC_O), HYPR(KC_P),
        KC_HOME,    KC_PGUP,    KC_PGDN,    KC_END
    ),
    [_LCAG] = LAYOUT(
        KC_NUM,     TO(_HYPR),  TO(_LCAG),  TO(_MEH),
        LCAG(KC_A), LCAG(KC_B), LCAG(KC_C), LCAG(KC_D),
        LCAG(KC_E), LCAG(KC_F), LCAG(KC_G), LCAG(KC_H),
        LCAG(KC_I), LCAG(KC_J), LCAG(KC_K), LCAG(KC_L),
        LCAG(KC_M), LCAG(KC_N), LCAG(KC_O), LCAG(KC_P),
        KC_HOME,    KC_PGUP,    KC_PGDN,    KC_END
    ),
    [_MEH] = LAYOUT(
        KC_NUM,     TO(_HYPR),  TO(_LCAG),  TO(_MEH),
        MEH(KC_A),  MEH(KC_B),  MEH(KC_C),  MEH(KC_D),
        MEH(KC_E),  MEH(KC_F),  MEH(KC_G),  MEH(KC_H),
        MEH(KC_I),  MEH(KC_J),  MEH(KC_K),  MEH(KC_L),
        MEH(KC_M),  MEH(KC_N),  MEH(KC_O),  MEH(KC_P),
        KC_HOME,    KC_PGUP,    KC_PGDN,    KC_END
    ),
};

// H1 is KC_NUM. When pressed, toggle OS Num Lock *and* move to the
// appropriate numpad layer (predicted from the flipped state). This keeps the
// layer switch tied to the keypress rather than to host LED-state echoes,
// which can fire spuriously and would otherwise yank us out of _HYPR/_LCAG/_MEH.
bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (keycode == KC_NUM && record->event.pressed) {
        bool new_num_lock = !host_keyboard_led_state().num_lock;
        layer_move(new_num_lock ? _NUM_ON : _NUM_OFF);
    }
    return true;  // still let KC_NUM fire normally so the host toggles Num Lock
}

// Keep numpad-layer state in sync with OS Num Lock, but *only* when we are
// already on a numpad layer. Otherwise external Num Lock changes (or spurious
// LED-state echoes from the host) would pull us off _HYPR/_LCAG/_MEH.
bool led_update_user(led_t led_state) {
    uint8_t layer = get_highest_layer(layer_state);
    if (layer == _NUM_ON || layer == _NUM_OFF) {
        layer_move(led_state.num_lock ? _NUM_ON : _NUM_OFF);
    }
    return true;
}

// === Per-layer top-row RGB indicators ===

// LED indices for the four top-row keys (derived from macropad.c's
// g_led_config matrix-to-LED table + macropad.h's LAYOUT macro).
#define LED_H1  5   // Num Lock / numpad layer toggle
#define LED_H2  2   // TO(_HYPR)
#define LED_H3 22   // TO(_LCAG)
#define LED_H4 17   // TO(_MEH)

// Theme colors                R    G    B
#define CLR_CYAN            0, 180, 180
#define CLR_GREEN           0, 180,   0
#define CLR_PURPLE        128,   0, 255
#define CLR_BLUE            0,  80, 255
#define CLR_AMBER         255, 160,   0

static inline void set_color_scaled(uint8_t index, uint8_t r, uint8_t g, uint8_t b) {
    uint8_t val = rgb_matrix_get_val();
    rgb_matrix_set_color(index,
        (uint8_t)((uint16_t)r * val / 255),
        (uint8_t)((uint16_t)g * val / 255),
        (uint8_t)((uint16_t)b * val / 255));
}

// Only the active layer's indicator key is lit; other top-row keys stay dark.
bool rgb_matrix_indicators_advanced_user(uint8_t led_min, uint8_t led_max) {
    for (uint8_t i = led_min; i < led_max; i++) {
        rgb_matrix_set_color(i, 0, 0, 0);
    }

    switch (get_highest_layer(layer_state)) {
        case _NUM_ON:
            set_color_scaled(LED_H1, CLR_CYAN);
            break;
        case _NUM_OFF:
            set_color_scaled(LED_H1, CLR_GREEN);
            break;
        case _HYPR:
            set_color_scaled(LED_H2, CLR_PURPLE);
            break;
        case _LCAG:
            set_color_scaled(LED_H3, CLR_BLUE);
            break;
        case _MEH:
            set_color_scaled(LED_H4, CLR_AMBER);
            break;
    }

    return false;
}

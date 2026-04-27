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
//   Top row (H1..H4): tap-dance layer switches on every layer.
//     Tap  = layer action (H1 toggles Num Lock, H2..H4 = TO(_HYPR/_LCAG/_MEH)).
//     Hold = send KC_F14..KC_F17 so the host (Emacs) can show the cheatsheet.
//            (KC_F17 for the Num key — F13 maps to XF86Tools, which GNOME grabs.)
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

// Top-row layer keys are tap-dance: tap = layer action, hold = show cheatsheet.
// Hold sends F14..F17 to the host; Emacs binds those to display the per-layer
// cheatsheet in a new frame. (Num uses F17 instead of F13 because F13 maps to
// XF86Tools under XKB, which GNOME intercepts.)
enum tap_dances {
    TD_NUM,
    TD_HYPR,
    TD_LCAG,
    TD_MEH,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [_NUM_ON] = LAYOUT(
        TD(TD_NUM), TD(TD_HYPR),TD(TD_LCAG),TD(TD_MEH),
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
        TD(TD_NUM), TD(TD_HYPR),TD(TD_LCAG),TD(TD_MEH),
        _______,    _______,    _______,    _______,
        _______,    _______,    _______,    _______,
        _______,    _______,    _______,    _______,
        _______,    _______,    _______,    _______,
        KC_HOME,    KC_PGUP,    KC_PGDN,    KC_END
    ),
    [_HYPR] = LAYOUT(
        TD(TD_NUM), TD(TD_HYPR),TD(TD_LCAG),TD(TD_MEH),
        HYPR(KC_A), HYPR(KC_B), HYPR(KC_C), HYPR(KC_D),
        HYPR(KC_E), HYPR(KC_F), HYPR(KC_G), HYPR(KC_H),
        HYPR(KC_I), HYPR(KC_J), HYPR(KC_K), HYPR(KC_L),
        HYPR(KC_M), HYPR(KC_N), HYPR(KC_O), HYPR(KC_P),
        KC_HOME,    KC_PGUP,    KC_PGDN,    KC_END
    ),
    [_LCAG] = LAYOUT(
        TD(TD_NUM), TD(TD_HYPR),TD(TD_LCAG),TD(TD_MEH),
        LCAG(KC_A), LCAG(KC_B), LCAG(KC_C), LCAG(KC_D),
        LCAG(KC_E), LCAG(KC_F), LCAG(KC_G), LCAG(KC_H),
        LCAG(KC_I), LCAG(KC_J), LCAG(KC_K), LCAG(KC_L),
        LCAG(KC_M), LCAG(KC_N), LCAG(KC_O), LCAG(KC_P),
        KC_HOME,    KC_PGUP,    KC_PGDN,    KC_END
    ),
    [_MEH] = LAYOUT(
        TD(TD_NUM), TD(TD_HYPR),TD(TD_LCAG),TD(TD_MEH),
        MEH(KC_A),  MEH(KC_B),  MEH(KC_C),  MEH(KC_D),
        MEH(KC_E),  MEH(KC_F),  MEH(KC_G),  MEH(KC_H),
        MEH(KC_I),  MEH(KC_J),  MEH(KC_K),  MEH(KC_L),
        MEH(KC_M),  MEH(KC_N),  MEH(KC_O),  MEH(KC_P),
        KC_HOME,    KC_PGUP,    KC_PGDN,    KC_END
    ),
};

// === Tap dance: tap = layer/num-lock action, hold = send Fn-key for cheatsheet ===
//
// `state->pressed` inside the *finished* callback is true if the key is still
// held when QMK resolves the dance (i.e. TAPPING_TERM elapsed without release).
// We treat that as "hold" and fire the cheatsheet keycode. Otherwise the user
// released first, so it's a tap and we perform the layer/num-lock action.

static void td_num_finished(tap_dance_state_t *state, void *user_data) {
    if (state->count != 1) return;
    if (state->pressed) {
        tap_code(KC_F17);
    } else {
        // Replicate the original row-1 KC_NUM behavior: predict the new
        // num-lock state, send KC_NUM to the host, and move to the matching
        // numpad layer. tap_code() does not pass through process_record_user,
        // so we must do the layer move ourselves here.
        bool new_num_lock = !host_keyboard_led_state().num_lock;
        tap_code(KC_NUM);
        layer_move(new_num_lock ? _NUM_ON : _NUM_OFF);
    }
}

static void td_hypr_finished(tap_dance_state_t *state, void *user_data) {
    if (state->count != 1) return;
    if (state->pressed) tap_code(KC_F14);
    else                layer_move(_HYPR);
}

static void td_lcag_finished(tap_dance_state_t *state, void *user_data) {
    if (state->count != 1) return;
    if (state->pressed) tap_code(KC_F15);
    else                layer_move(_LCAG);
}

static void td_meh_finished(tap_dance_state_t *state, void *user_data) {
    if (state->count != 1) return;
    if (state->pressed) tap_code(KC_F16);
    else                layer_move(_MEH);
}

tap_dance_action_t tap_dance_actions[] = {
    [TD_NUM]  = ACTION_TAP_DANCE_FN_ADVANCED(NULL, td_num_finished,  NULL),
    [TD_HYPR] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, td_hypr_finished, NULL),
    [TD_LCAG] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, td_lcag_finished, NULL),
    [TD_MEH]  = ACTION_TAP_DANCE_FN_ADVANCED(NULL, td_meh_finished,  NULL),
};

// Row-2 KC_NUM (within the numpad cluster) still fires through the matrix and
// is handled here: toggle OS Num Lock and move to the matching numpad layer.
bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (keycode == KC_NUM && record->event.pressed) {
        bool new_num_lock = !host_keyboard_led_state().num_lock;
        layer_move(new_num_lock ? _NUM_ON : _NUM_OFF);
    }
    return true;
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

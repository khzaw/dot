#include QMK_KEYBOARD_H

#define _QWERTY 0
#define _LOWER 1
#define _RAISE 2

enum custom_keycodes {
  QWERTY = SAFE_RANGE,
  LOWER,
  RAISE
};

#define KC_ KC_TRNS
#define KC_LOWR MO(_LOWER) // Lower layer
#define KC_RASE MO(_RAISE) // Raise layer
#define KC_TGLW TG(_LOWER) // toggle lower
#define KC_TGRS TG(_RAISE) // toggle raise


const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

    // QWERTY LAYER
  //,----+----+----+----+----+----.              ,----+----+----+----+----+----.
  //|----+----+----+----+----+----|              |----+----+----+----+----+----|
  // LCTL, A  , S  , D  , F  , G  ,                H  , J  , K  , L  ,SCLN,QUOT,
  //|----+----+----+----+----+----+----.    ,----|----+----+----+----+----+----|
  // LSPO, Z  , X  , C  , V  , B  ,LALT ,  BSLASH, N , M  ,COMM,DOT ,SLSH ,RSPC,
  //`----+----+----+--+-+----+----+----/    \----+----+----+----+----+----+----'
  //                   RASE,LGUI,ENT ,         SPC ,RALT,TGLW
  //                  `----+----+----'        `----+----+----'

  [_QWERTY] = LAYOUT(
  //┌────────┬────────┬────────┬────────┬────────┬────────┐                          ┌────────┬────────┬────────┬────────┬────────┬────────┐
     KC_ESC,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,                               KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_BSPC,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤
     KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,                               KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_MINS,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤
     KC_LCTL, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,                               KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
  //├────────┼────────┼────────┼────────┼────────┼────────┼────────┐        ┌────────┼────────┼────────┼────────┼────────┼────────┼────────┤
     SC_LSPO, KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    KC_LALT,         KC_BSLASH,KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RSPC,
  //└────────┴────────┴────────┴───┬────┴───┬────┴───┬────┴───┬────┘        └───┬────┴───┬────┴───┬────┴───┬────┴────────┴────────┴────────┘
                                    KC_LGUI, KC_LOWR, KC_ENTER,                   KC_SPC,  KC_RASE, KC_RALT
                                // └────────┴────────┴────────┘                 └────────┴────────┴────────┘
  ),

    // LOWER Layer
   //,----+----+----+----+----+----.              ,----+----+----+----+----+----.
  // TILD,EXLM, AT ,HASH,DLR ,PERC,               CIRC,AMPR,ASTR,LPRN,RPRN,BSPC,
  //|----+----+----+----+----+----|              |----+----+----+----+----+----|
  //     ,    ,    ,    ,    ,LBRC,               RBRC, P7 , P8 , P9 ,    ,PLUS,
  //|----+----+----+----+----+----|              |----+----+----+----+----+----|
  //     ,HOME,PGUP,PGDN,END,LPRN,               RPRN, P4 , P5 , P6 ,MINS,PIPE,
  //|----+----+----+----+----+----+----.    ,----|----+----+----+----+----+----|
  //     ,    ,    ,    ,    ,    ,    ,         ,    , P1 , P2 , P3 ,EQL ,UNDS ,
  //`----+----+----+--+-+----+----+----/    \----+----+----+----+----+----+----'
  //                       ,    ,DEL ,        BSPC ,    , P0
  //                  `----+----+----'        `----+----+----'


  [_LOWER] = LAYOUT(
  //┌────────┬────────┬────────┬────────┬────────┬────────┐                          ┌────────┬────────┬────────┬────────┬────────┬────────┐
      KC_GRV, KC_EXLM, KC_AT,  KC_HASH,  KC_DLR,  KC_PERC,                            KC_CIRC, KC_AMPR, KC_ASTR, KC_LPRN , KC_RPRN,  KC_DEL,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤
     _______, KC_BRMD, KC_BRMU, _______, _______, KC_LBRC,                            KC_RBRC, _______, _______,  _______, _______, KC_PLUS,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤
     _______, KC_VOLD, KC_VOLU, KC_MUTE, _______, KC_    ,                            KC_LEFT, KC_DOWN, KC_UP, KC_RIGHT,_______,   KC_BSLS,
  //├────────┼────────┼────────┼────────┼────────┼────────┼────────┐        ┌────────┼────────┼────────┼────────┼────────┼────────┼────────┤
     _______, KC_MPRV, KC_MPLY, KC_MNXT, _______ ,_______, _______,          _______, _______, _______, _______, _______,  KC_EQL, KC_UNDS,
  //└────────┴────────┴────────┴───┬────┴───┬────┴───┬────┴───┬────┘        └───┬────┴───┬────┴───┬────┴───┬────┴────────┴────────┴────────┘
                                    KC_,     KC_,     KC_,                       KC_GRV,      KC_,     KC_
                                // └────────┴────────┴────────┘                 └────────┴────────┴────────┘
  ),

  [_RAISE] = LAYOUT(
  //┌────────┬────────┬────────┬────────┬────────┬────────┐                          ┌────────┬────────┬────────┬────────┬────────┬────────┐
     KC_F12  ,  KC_F1 , KC_F2  , KC_F3  , KC_F4  ,  KC_F5 ,                            KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  KC_F11,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤
     RGB_TOG , BL_TOGG, _______, _______, _______, KC_LCBR,                           KC_RCBR, _______, _______, _______, _______, _______,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤
     RGB_MOD , BL_STEP, _______, _______, _______, QK_RBT,                            _______, _______, _______, _______, _______, KC_PIPE,
  //├────────┼────────┼────────┼────────┼────────┼────────┼────────┐        ┌────────┼────────┼────────┼────────┼────────┼────────┼────────┤
     RGB_RMOD, _______, _______, _______, _______, _______, QK_BOOT,         _______, _______, _______,  _______, _______, _______, _______,
  //└────────┴────────┴────────┴───┬────┴───┬────┴───┬────┴───┬────┘        └───┬────┴───┬────┴───┬────┴───┬────┴────────┴────────┴────────┘
                                    _______, _______, _______,                   _______, _______, _______
                                // └────────┴────────┴────────┘                 └────────┴────────┴────────┘
 ),

/*   [_ADJUST] = LAYOUT( */
/*   //┌────────┬────────┬────────┬────────┬────────┬────────┐                          ┌────────┬────────┬────────┬────────┬────────┬────────┐ */
/*      _______, _______, _______, _______, _______, _______,                            _______, _______, _______, _______, _______, _______, */
/*   //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤ */
/*      _______, _______, _______, _______, _______, _______,                            _______, _______, _______, _______, _______, _______, */
/*   //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤ */
/*      _______, _______, _______, _______, _______, _______,                            _______, _______, _______, _______, _______, _______, */
/*   //├────────┼────────┼────────┼────────┼────────┼────────┼────────┐        ┌────────┼────────┼────────┼────────┼────────┼────────┼────────┤ */
/*      _______, _______, _______, _______, _______, _______, _______,          _______, _______, _______, _______, _______, _______, _______, */
/*   //└────────┴────────┴────────┴───┬────┴───┬────┴───┬────┴───┬────┘        └───┬────┴───┬────┴───┬────┴───┬────┴────────┴────────┴────────┘ */
/*                                     _______, _______, _______,                   _______, _______, _______ */
/*                                 // └────────┴────────┴────────┘                 └────────┴────────┴────────┘ */
/*   ) */
};

/* bool process_record_user(uint16_t keycode, keyrecord_t *record) { */
/*   switch (keycode) { */
/*     case QWERTY: */
/*       if (record->event.pressed) { */
/*         set_single_persistent_default_layer(_QWERTY); */
/*       } */
/*       return false; */
/*       break; */
/*     case LOWER: */
/*       if (record->event.pressed) { */
/*         layer_on(_LOWER); */
/*         update_tri_layer(_LOWER, _RAISE, _ADJUST); */
/*       } else { */
/*         layer_off(_LOWER); */
/*         update_tri_layer(_LOWER, _RAISE, _ADJUST); */
/*       } */
/*       return false; */
/*       break; */
/*     case RAISE: */
/*       if (record->event.pressed) { */
/*         layer_on(_RAISE); */
/*         update_tri_layer(_LOWER, _RAISE, _ADJUST); */
/*       } else { */
/*         layer_off(_RAISE); */
/*         update_tri_layer(_LOWER, _RAISE, _ADJUST); */
/*       } */
/*       return false; */
/*       break; */
/*     case ADJUST: */
/*       if (record->event.pressed) { */
/*         layer_on(_ADJUST); */
/*       } else { */
/*         layer_off(_ADJUST); */
/*       } */
/*       return false; */
/*       break; */
/*   } */
/*   return true; */
/* } */

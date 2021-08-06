# Encoders

Basic encoders are supported by adding this to your `rules.mk`:

```make
ENCODER_ENABLE = yes
```

and this to your `config.h`:

```c
#define ENCODERS_PAD_A { B12 }
#define ENCODERS_PAD_B { B13 }
```

Each PAD_A/B variable defines an array so multiple encoders can be defined, e.g.:

```c
#define ENCODERS_PAD_A { encoder1a, encoder2a }
#define ENCODERS_PAD_B { encoder1b, encoder2b }
```

If your encoder's clockwise directions are incorrect, you can swap the A & B pad definitions.  They can also be flipped with a define:

```c
#define ENCODER_DIRECTION_FLIP
```

Additionally, the resolution, which defines how many pulses the encoder registers between each detent, can be defined with:

```c
#define ENCODER_RESOLUTION 4
```

It can also be defined per-encoder, by instead defining:

```c
#define ENCODER_RESOLUTIONS { 4, 2 }
```

## Split Keyboards

If you are using different pinouts for the encoders on each half of a split keyboard, you can define the pinout (and optionally, resolutions) for the right half like this:

```c
#define ENCODERS_PAD_A_RIGHT { encoder1a, encoder2a }
#define ENCODERS_PAD_B_RIGHT { encoder1b, encoder2b }
#define ENCODER_RESOLUTIONS_RIGHT { 2, 4 }
```

## Callbacks

The callback functions can be inserted into your `<keyboard>.c`:

```c
bool encoder_update_kb(uint8_t index, bool clockwise) {
    return encoder_update_user(index, clockwise);
}
```

or `keymap.c`:

```c
bool encoder_update_user(uint8_t index, bool clockwise) {
    if (index == 0) { /* First encoder */
        if (clockwise) {
            tap_code(KC_PGDN);
        } else {
            tap_code(KC_PGUP);
        }
    } else if (index == 1) { /* Second encoder */
        if (clockwise) {
            tap_code(KC_DOWN);
        } else {
            tap_code(KC_UP);
        }
    }
    return true;
}
```

!> If you return `true`, this will allow the keyboard level code to run, as well.  Returning `false` will override the keyboard level code.  Depending on how the keyboard level function is set up. 

## Hardware

The A an B lines of the encoders should be wired directly to the MCU, and the C/common lines should be wired to ground.

## Multiple Encoders

Multiple encoders may share pins so long as each encoder has a distinct pair of pins. 

For example you can support two encoders using only 3 pins like this
```
#define ENCODERS_PAD_A { B1, B1 }
#define ENCODERS_PAD_B { B2, B3 }
```

You could even support three encoders using only three pins (one per encoder) however in this configuration, rotating two encoders which share pins simultaneously will often generate incorrect output. For example:
```
#define ENCODERS_PAD_A { B1, B1, B2 }
#define ENCODERS_PAD_B { B2, B3, B3 }
```
Here rotating Encoder 0 `B1 B2` and Encoder 1 `B1 B3` could be interpreted as rotating Encoder 2 `B2 B3` or `B3 B2` depending on the timing. This may still be a useful configuration depending on your use case 

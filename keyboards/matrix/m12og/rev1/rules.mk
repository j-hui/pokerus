# MCU name
MCU = STM32F103

MCU_LDSCRIPT = m12og_v1

BOARD = m12og_v1

# Build Options
#   change yes to no to disable
#
BOOTMAGIC_ENABLE = no       # Bootmagic disabled because Matrix bootlaoder uses it's own reset mechanism
MOUSEKEY_ENABLE = no        # Mouse keys
EXTRAKEY_ENABLE = yes       # Audio control and System control
CONSOLE_ENABLE = no         # Console for debug
COMMAND_ENABLE = no         # Commands for debug and configuration
# Do not enable SLEEP_LED_ENABLE. it uses the same timer as BACKLIGHT_ENABLE
SLEEP_LED_ENABLE = no       # Breathing sleep LED during USB suspend
# if this doesn't work, see here: https://github.com/tmk/tmk_keyboard/wiki/FAQ#nkro-doesnt-work
NKRO_ENABLE = no            # USB Nkey Rollover
BACKLIGHT_ENABLE = no       # Enable keyboard backlight functionality
RGBLIGHT_ENABLE = yes       # HAS TO BE ON! Otherwise the custom matrix doesn't work
BLUETOOTH_ENABLE = no       # Enable Bluetooth
AUDIO_ENABLE = no           # Audio output

CUSTOM_MATRIX = lite
SRC += matrix.c

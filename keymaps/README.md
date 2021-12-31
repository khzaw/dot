- Configure QMK config


```
qmk config user.keymap=khzaw
qmk config user.keyboard=<keyboard_name>   # ymdk/wingshs for example
```

- Symlink the file to `qmk_firmware/keyboards/<keyboard_name>/keymaps/khzaw/keymap.c`.
- Compile keymap file

```
qmk compile
```
- Put keyboard into DFU mode (usually long-press RESET key) and flash keymap

```
qmk flash
```





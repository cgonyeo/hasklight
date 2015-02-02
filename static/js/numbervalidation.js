function handlechange(input, low, high, allow_decimal) {
    if (input.value < low) input.value = low;
    if (input.value > high) input.value = high;
    if (allow_decimal) {
        input.value = input.value.replace(/[^0-9\.]/g,'');
    } else {
        input.value = input.value.replace(/[^0-9]/g,'');
    }
}

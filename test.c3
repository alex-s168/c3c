
fn int max(int a, int b) @export 
{
    int res;
    if (a > b) {
        res = a;
    } else {
        res = b;
    }
    return res;
}

fn void strcpy(char* dest, char* src) @export 
{
    for (usz i = 0; src[i] != '\0'; i ++)
        dest[i] = src[i];
}

fn int max2(int a, int b) @export
{
    if (a > b) {
        return a;
    }
    else {
        return b;
    }
}

fn int max3(int a, int b) @export
{
    if (a > b) {
        return a;
    }
    return b;
}

fn int ptrtest(int* z) @export
{
    return z[2];
}

fn int addfirst4(int* z) @export 
{
    return z[0] + z[1] + z[2] + z[3];
}

fn int assignments(int y) @export 
{
    int x = 0;
    x += y;
    x -= y;
    x %= y;
    x /= y;
    x *= y;
    x <<= y;
    x >>= y;
    x |= y;
    x &= y;
    return (int) x;
}

fn int sum(int* arr, int len) @export 
{
    int sum = 0;
    for (int i = 0; i < len; i ++)
    {
        sum += arr[i];
    }
    return sum;
}

fn void eltwise_add(int* o, int* a, int* b, usz len) @export 
{
    for (usz i = 0; i < len; i ++)
    {
        o[i] = a[i] + b[i];
    }
}

fn void colwise_reduce_sum(int* dest, int* a, usz w, usz h) @export 
{
    for (usz col = 0; col < w; col ++)
    {
        int sum = 0;
        for (usz row = 0; row < h; row ++)
        {
            sum += a[col * w + row];
        }
        dest[col] = sum;
    }
}

fn usz test(int a, int b) @export 
{
    return ((usz) (&a)) + b;
}

/*
struct Vec2i 
{
    int x;
    int y;
}

fn int test2() @export 
{
    Vec2i vec = Vec2i { .x = 1, .y = 2 };
    Vec2i copy = vec;
    int y = vec.y;
    return y;
}*/

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

fn int sum(int* arr, int len) @export 
{
    int sum = 0;
    for (int i = 0; i < len; i ++)
    {
        sum += arr[i];
    }
    return sum;
}


def example_filter(msg):
    print("MSG")
    return (msg == 1)


def example_transform(msg):
    if not (isinstance(msg, int) and msg < 4):
        raise Exception
    return str(msg)

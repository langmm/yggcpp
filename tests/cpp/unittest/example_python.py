
def example_filter(msg):
    print("MSG")
    return (msg == 1)


def example_transform(msg):
    if not (isinstance(msg, int) and msg < 4):
        raise Exception
    return str(msg)


def example_filter_error(msg):
    if msg is not None:
        raise Exception("Test error")
    return False


def example_transform_error(msg):
    if msg is not None:
        raise Exception("Test error")
    return False


def example_filter_invalid(msg):
    return "hello"

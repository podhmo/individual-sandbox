def gen_pred(b, message):
    def pred():
        print(message)
        return b

    return pred


ps = [gen_pred(True, 0), gen_pred(False, 1), gen_pred(True, 2)]
print(all(p() for p in ps))

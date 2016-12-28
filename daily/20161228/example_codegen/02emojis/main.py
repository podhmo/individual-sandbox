from schema import Emojis
print(Emojis().load({"+1": "hai", "-1": "hoi", "8ball": "o_0"}))
# UnmarshalResult(data={'x1': 'hai', 'n8ball': 'o_0', 'x_1': 'hoi'}, errors={})
data, errs = Emojis().load({"+1": "hai", "-1": "hoi", "8ball": "o_0"})
print(Emojis().dump(data))
# MarshalResult(data={'8ball': 'o_0', '-1': 'hoi', '+1': 'hai'}, errors={})

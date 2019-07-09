from fastapi.encoders import jsonable_encoder

print(jsonable_encoder({"foo": "bar"}))

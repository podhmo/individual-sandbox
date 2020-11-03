## marshmallow codegen

どういう可能性があるんだろう

- toplevelと利用するものと言う二段階がある。
- object,array,primitive
- object,array,object
- object,array,ref
- object,array,array
- object,dict,primitive
- object,dict,object
- object,additionals
- object,object
- object,ref
- object,self
- object,primitive
- primitive
- array,primitive
- array,object
- array,ref

ところどころ壊れているな。。

### v3

- liftingが機能していない。


ln -s ../../shapes/v2 v2src
ln -s ../../shapes/v3 v3src
ln -s ../../shapes/expected v2dst
ln -s ../../shapes/expected v3dst

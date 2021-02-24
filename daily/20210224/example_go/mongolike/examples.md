examples

```
db.inventory.find( { qty: { $gt: 20 } } )

db.inventory.find( { price: { $ne: 1.99, $exists: true } } )

db.inventory.find( {
    $and: [
        { $or: [ { qty: { $lt : 10 } }, { qty : { $gt: 50 } } ] },
        { $or: [ { sale: true }, { price : { $lt : 5 } } ] }
    ]
} )
```

import "@typespec/http";

using TypeSpec.Http;

@service({
  title: "Pet Store",
})
@server("https://example.com", "Single server endpoint")
namespace PetStore;

model Pet {
  id: int32;

  @minLength(1)
  name: string;

  @minValue(0)
  @maxValue(100)
  age: int32;

  kind: petType;
}

enum petType {
  dog: "dog",
  cat: "cat",
  fish: "fish",
  bird: "bird",
  reptile: "reptile",
}

@route("/pets")
namespace Pets {
  @get
  op listPets(): {
    @statusCode statusCode: 200;
    @body pets: Pet[];
  };

  @get
  op getPet(@path petId: int32): {
    @statusCode statusCode: 200;
    @body pet: Pet;
  } | {
    @statusCode statusCode: 404;
  };

  @post
  op createPet(@body pet: Pet): {
    @statusCode statusCode: 201;
    @body newPet: Pet;
  } | {
    @statusCode statusCode: 202;
    @body acceptedPet: Pet;
  };

  @put
  op updatePet(@path petId: int32, @body pet: Pet): {
    @statusCode statusCode: 200;
    @body updatedPet: Pet;
  } | {
    @statusCode statusCode: 404;
  };

  @delete
  op deletePet(@path petId: int32): {
    @statusCode statusCode: 204;
  };
}
from __future__ import annotations
import typing as t
import typing_extensions as tx

# https://editor.swagger.io/
int64 = t.NewType("int64", int)
int32 = t.NewType("int32", int)
dateTime = t.NewType("date-time", str)

OrderStatus = tx.Literal["placed", "approved", "delivered"]


class Order:
    id: int64
    petId: int64
    quantity: int32
    shipDate: dateTime
    status: OrderStatus
    complete: bool = False


class Category:
    id: int64
    name: str


class User:
    id: int64
    username: str
    firstName: str
    lastName: str
    email: str
    password: str
    phone: str
    usrStatus: int32


class Tag:
    id: int64
    name: str


PetStatus = tx.Literal["available", "pending", "sold"]


class Pet:
    id: int64
    category: Category
    name: str
    photoUrls: t.List[str]
    tags: t.List[Tag]
    status: PetStatus


class ApiResponse:
    code: int32
    type: str
    message: str


## API
from prestring.codeobject import Symbol  # noqa 401

App = Symbol("App")
File = t.NewType("File", object)

Query = t.TypeVar("Query")
Path = t.TypeVar("Path")
FormData = t.TypeVar("FormData")
Header = t.TypeVar("Header")

api = App()


@api.petstore_auth(["write:pets", "read:pets"])
class PetService:
    @api.post("/pet")
    def addPet(self, body: Pet) -> None:
        """Pet object that needs to be added to the store"""
        # "405":
        #   description: "Invalid input"
        pass

    @api.put("/pet")
    def updatePet(self, body: Pet) -> None:
        """Update an existing pet"""
        # "400":
        #   description: "Invalid ID supplied"
        # "404":
        #   description: "Pet not found"
        # "405":
        #   description: "Validation exceptio
        pass

    @api.get("/et/findByStatus")
    def findPetsByStatus(self, status: Query[PetStatus]) -> t.List[Pet]:
        """Finds Pets by status

        Multiple status values can be provided with comma separated strings
"""
        # "200":
        #   description: "successful operation"
        #   schema:
        #     type: "array"
        #     items:
        #       $ref: "#/definitions/Pet"
        # "400":
        #   description: "Invalid status value"
        pass

    @api.get("/pet/findByTags")
    def findPetsByTags(self, tags: Query[str]) -> t.List[Pet]:
        """Finds Pets by tags

        Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
        """
        # "200":
        #   description: "successful operation"
        #   schema:
        #     type: "array"
        #     items:
        #       $ref: "#/definitions/Pet"
        # "400":
        #   description: "Invalid tags value"
        pass

    @api.get("/pet/{petId}")
    def getPetById(self, petId: Path[int64]) -> Pet:
        # petID.description = "ID of pet to return"
        # "200":
        #   description: "successful operation"
        #   schema:
        #     $ref: "#/definitions/Pet"
        # "400":
        #   description: "Invalid ID supplied"
        # "404":
        #   description: "Pet not found"
        pass

    @api.post("/pet/{petId}")
    def updatePetWithFormData(
        self, petId: Path[int64], name: FormData[str], status: t.Optional[FormData[str]]
    ):
        # "405":
        #   description: "Invalid input"
        pass

    @api.delete("/pet/{petId}")
    def deletePet(self, pet_id: Path[int64], api_key: t.Optional[Header[str]]):
        # "400":
        #   description: "Invalid ID supplied"
        # "404":
        #   description: "Pet not found"
        pass

    @api.post("/pet/{petId}/uploadImage")
    def uploadFile(
        self,
        pet_id: Path[int64],
        additionalmetadata: t.Optional[FormData[str]],
        file=t.Optional[FormData[File]],
    ) -> ApiResponse:
        # "200":
        #   description: "successful operation"
        #   schema:
        #     $ref: "#/definitions/ApiResponse"
        pass


class StoreService:
    @api.get("/store/invetory")
    def getInventory(self) -> t.Dict[str, int]:
        """Returns pet inventories by status

        Returns a map of status codes to quantities
        """
        # "200":
        #   description: "successful operation"
        #   schema:
        #     type: "object"
        #     additionalProperties:
        #       type: "integer"
        #       format: "int32"
        pass

    @api.post("/store/order")
    def placeOrder(self, body: Order) -> Order:
        """Place an order for a pet"""
        # "200":
        #   description: "successful operation"
        #   schema:
        #     $ref: "#/definitions/Order"
        # "400":
        #   description: "Invalid Order"

        pass

    # orderId
    # - maximum: 10.0
    # - minimum: 1.0

    @api.get("/store/order/{orderId}")
    def getOrderById(self, orderId: Path[int64]) -> Order:
        # "200":
        #   description: "successful operation"
        #   schema:
        #     $ref: "#/definitions/Order"
        # "400":
        #   description: "Invalid ID supplied"
        # "404":
        #   description: "Order not found"
        pass

    @api.delete("/store/order/{orderId}")
    def deleteOrder(self, orderId: Path[int64]):
        # "400":
        #   description: "Invalid ID supplied"
        # "404":
        #   description: "Order not found"
        pass


# # todo: user
#   /user:
#     post:
#       tags:
#       - "user"
#       summary: "Create user"
#       description: "This can only be done by the logged in user."
#       operationId: "createUser"
#       produces:
#       - "application/json"
#       parameters:
#       - in: "body"
#         name: "body"
#         description: "Created user object"
#         required: true
#         schema:
#           $ref: "#/definitions/User"
#       responses:
#         default:
#           description: "successful operation"
#   /user/createWithArray:
#     post:
#       tags:
#       - "user"
#       summary: "Creates list of users with given input array"
#       description: ""
#       operationId: "createUsersWithArrayInput"
#       produces:
#       - "application/json"
#       parameters:
#       - in: "body"
#         name: "body"
#         description: "List of user object"
#         required: true
#         schema:
#           type: "array"
#           items:
#             $ref: "#/definitions/User"
#       responses:
#         default:
#           description: "successful operation"
#   /user/createWithList:
#     post:
#       tags:
#       - "user"
#       summary: "Creates list of users with given input array"
#       description: ""
#       operationId: "createUsersWithListInput"
#       produces:
#       - "application/json"
#       parameters:
#       - in: "body"
#         name: "body"
#         description: "List of user object"
#         required: true
#         schema:
#           type: "array"
#           items:
#             $ref: "#/definitions/User"
#       responses:
#         default:
#           description: "successful operation"
#   /user/login:
#     get:
#       tags:
#       - "user"
#       summary: "Logs user into the system"
#       description: ""
#       operationId: "loginUser"
#       produces:
#       - "application/json"
#       parameters:
#       - name: "username"
#         in: "query"
#         description: "The user name for login"
#         required: true
#         type: "string"
#       - name: "password"
#         in: "query"
#         description: "The password for login in clear text"
#         required: true
#         type: "string"
#       responses:
#         "200":
#           description: "successful operation"
#           schema:
#             type: "string"
#           headers:
#             X-Rate-Limit:
#               type: "integer"
#               format: "int32"
#               description: "calls per hour allowed by the user"
#             X-Expires-After:
#               type: "string"
#               format: "date-time"
#               description: "date in UTC when token expires"
#         "400":
#           description: "Invalid username/password supplied"
#   /user/logout:
#     get:
#       tags:
#       - "user"
#       summary: "Logs out current logged in user session"
#       description: ""
#       operationId: "logoutUser"
#       produces:
#       - "application/json"
#       parameters: []
#       responses:
#         default:
#           description: "successful operation"
#   /user/{username}:
#     get:
#       tags:
#       - "user"
#       summary: "Get user by user name"
#       description: ""
#       operationId: "getUserByName"
#       produces:
#       - "application/json"
#       parameters:
#       - name: "username"
#         in: "path"
#         description: "The name that needs to be fetched. Use user1 for testing. "
#         required: true
#         type: "string"
#       responses:
#         "200":
#           description: "successful operation"
#           schema:
#             $ref: "#/definitions/User"
#         "400":
#           description: "Invalid username supplied"
#         "404":
#           description: "User not found"
#     put:
#       tags:
#       - "user"
#       summary: "Updated user"
#       description: "This can only be done by the logged in user."
#       operationId: "updateUser"
#       produces:
#       - "application/json"
#       parameters:
#       - name: "username"
#         in: "path"
#         description: "name that need to be updated"
#         required: true
#         type: "string"
#       - in: "body"
#         name: "body"
#         description: "Updated user object"
#         required: true
#         schema:
#           $ref: "#/definitions/User"
#       responses:
#         "400":
#           description: "Invalid user supplied"
#         "404":
#           description: "User not found"
#     delete:
#       tags:
#       - "user"
#       summary: "Delete user"
#       description: "This can only be done by the logged in user."
#       operationId: "deleteUser"
#       produces:
#       - "application/json"
#       parameters:
#       - name: "username"
#         in: "path"
#         description: "The name that needs to be deleted"
#         required: true
#         type: "string"
#       responses:
#         "400":
#           description: "Invalid username supplied"
#         "404":
#           description: "User not found"

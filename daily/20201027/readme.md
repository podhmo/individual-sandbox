## 気にしたかったこと

- controller,intructor -> custom response
- permission -> 内部の情報をどうやって外部に公開するか
- generate validator -> go/typesを使ったshape。
- bubbletea -> TUIを使ったUIの仮実装。
- sqsの投機的実行。

本当にほしいのはopenAPI?コアはintaractor何じゃないか？という検証がしたいのかもしれない。

## go go docの表示

go docの表示ってどうなってるんだっけ？
記憶が確かなら部分的にgo/printerを使っていたはず。

### 実行

internal/base.Run()で読んでいるだけなんだなー。go/docのmainを

```go

	cmd := exec.Command(cmdline[0], cmdline[1:]...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		Errorf("%v", err)
	}

```

んー。結構大変そう。

## 呼び出し関係を取り出す

- 関数定義 -> 関数名
- 関数呼び出し -> 関数名 / with receiver check

astの何を見れば良いんだろう？ `fmt.Printf()`
CallExprを見て、Fun.Sel.Nameが関数名。receiverはFun.X.Name
```
//  21  .  .  .  .  X: *ast.CallExpr {
//  22  .  .  .  .  .  Fun: *ast.SelectorExpr {
//  23  .  .  .  .  .  .  X: *ast.Ident {
//  24  .  .  .  .  .  .  .  NamePos: m/internal/s/s.go:17:2
//  25  .  .  .  .  .  .  .  Name: "fmt"
//  26  .  .  .  .  .  .  }
//  27  .  .  .  .  .  .  Sel: *ast.Ident {
//  28  .  .  .  .  .  .  .  NamePos: m/internal/s/s.go:17:6
//  29  .  .  .  .  .  .  .  Name: "Println"
//  30  .  .  .  .  .  .  }
//  31  .  .  .  .  .  }
//  32  .  .  .  .  .  Lparen: m/internal/s/s.go:17:13
//  33  .  .  .  .  .  Args: []ast.Expr (len = 1) {
//  34  .  .  .  .  .  .  0: *ast.BasicLit {
//  35  .  .  .  .  .  .  .  ValuePos: m/internal/s/s.go:17:14
//  36  .  .  .  .  .  .  .  Kind: STRING
//  37  .  .  .  .  .  .  .  Value: "\"hello\""
//  38  .  .  .  .  .  .  }
//  39  .  .  .  .  .  }
//  40  .  .  .  .  .  Ellipsis: -
//  41  .  .  .  .  .  Rparen: m/internal/s/s.go:17:21
//  42  .  .  .  .  }
//  43  .  .  .  }
```

### そこそこ頑張ったけど

以下がまだ無理

- 相互再帰
- 自分自身ではなく同じ型のfieldを呼んだとき
- ↑のmap,slice経由の呼び出し

go/typesの力を借りるしかないのでは？

### お手軽

忘れがち。

```
fset.File(ob.Pos()).Name // <filename>
fset.Position(ob.Pos()) // <filename>:<lineno>
```

### TypesInfo

なんだっけこれ。Defsは変数含めて全部か。これ使って型を取り出せるのかな。

```
type Info struct {
	Types map[ast.Expr]TypeAndValue

	Defs map[*ast.Ident]Object
	Uses map[*ast.Ident]Object

	Implicits map[ast.Node]Object
	Selections map[*ast.SelectorExpr]*Selection
	Scopes map[ast.Node]*Scope
	InitOrder []*Initializer
}

func (info *Info) ObjectOf(id *ast.Ident) Object
func (info *Info) TypeOf(e ast.Expr) Type
```

## 小ネタ

- その埋め込み本当に必要ですか？ (go-github)

## ほしいTUI

- /でのsearch, ?でのreverse search
- hjklでのカーソル移動
- ><でのexpand,unexpand
- qで閉じる
- fuzzy find
- enterでnodeをクリック
- そのノードをmarkdownとしてclipboardにコピー
- (gt,guでのタブ移動)
- (:XXXでのコマンド実行)

## go RBAC

普通にやる分にはなんとなくわかったけど。この辺が気になる感じ

- dynamic filter
- Role has role
- ARN like expression? e.g. arn:aws:iam::123456789012:user/Development/product_1234/*

  - `my:issue:iam::<repository name>:role/<name>/issue/<issueID>` ?

これをいい感じにreflect-openAPIの方にマージしていきたい感じ。

- https://nrslib.com/clean-architecture/

## OpenAPI authorization

- https://swagger.io/docs/specification/authentication/

OASで、scopeがOAuth2のときしか使えないのはおかしくない？と思っていたら、OAS3.1では使えるようになるっぽいな。

- https://github.com/OAI/OpenAPI-Specification/issues/1366

### 3.0

- define `#/components/securitySchemas`

```yaml
components:
  securitySchemes:
    ApiKeyAuth:
      type: apiKey
      in: header
      name: X-API-Key
    OAuth2:
      type: oauth2
      flows:
        authorizationCode:
          authorizationUrl: https://example.com/oauth/authorize
          tokenUrl: https://example.com/oauth/token
          scopes:
            read: Grants read access
            write: Grants write access
            admin: Grants access to admin operations


security:
  - ApiKeyAuth: []
  - OAuth2:
      - read
      - write

paths:
  /billing_info:
    get:
      summary: Gets the account billing info
      security:
        - OAuth2: [admin]   # Use OAuth with a different scope
      responses:
        '200':
          description: OK
        '401':
          description: Not authenticated
        '403':
          description: Access token does not have the required scope
  /ping:
    get:
      summary: Checks if the server is running
      security: []   # No security
      responses:
        '200':
          description: Server is up and running
        default:
          description: Something is wrong
```

ちなみに、(A and B) or (C and D) とかできる。

```
    security:    # (A AND B) OR (C AND D)
      - A
        B
      - C
        D
```

### 2.0

securityRequirementsを各endpointが持てて、securityDefinitionsを定義する感じ

securityRequirements

https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#securityRequirementObject

```yaml
security:
- petstore_auth:
  - write:pets
  - read:pets
```

securityDefinitions

https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#securityDefinitionsObject

```yaml
api_key:
  type: apiKey
  name: api_key
  in: header
petstore_auth:
  type: oauth2
  authorizationUrl: http://swagger.io/api/oauth/dialog
  flow: implicit
  scopes:
    write:pets: modify pets in your account
    read:pets: read your pets
```

## RBAC (role base access control)

- https://dev.to/bastianrob/rbac-in-rest-api-using-go-5gg0
- https://dev.to/sergey_telpuk/write-rbac-for-nestjs-1ike

この記事をもう少しいい感じになんないだろうか？

## permissionの例ないかな？

- https://docs.laminas.dev/laminas-permissions-acl/usage/
- https://aspnetboilerplate.com/Pages/Documents/Zero/Permission-Management

ACL

| Name  | Unique Permissions  | Inherit Permissions From |
| :--- | :--- | :--- |
| Guest  | View  | N/A |
| Staff  | Edit, Submit, Revise  | Guest |
| Editor  | Publish, Archive, Delete  | Staff |
| Administrator  | (Granted all access)  | N/A |

- user -> role -> permission
- inherit

これとは別に、queryで気にするものもある。
というか、認可で書きたいのはACLなんだろうか？

### もう１つ

role permissions

```csharp
public class RoleAppService : IRoleAppService
{
    private readonly RoleManager _roleManager;
    private readonly IPermissionManager _permissionManager;

    public RoleAppService(RoleManager roleManager, IPermissionManager permissionManager)
    {
        _roleManager = roleManager;
        _permissionManager = permissionManager;
    }

    public async Task UpdateRolePermissions(UpdateRolePermissionsInput input)
    {
        var role = await _roleManager.GetRoleByIdAsync(input.RoleId);
        var grantedPermissions = _permissionManager
            .GetAllPermissions()
            .Where(p => input.GrantedPermissionNames.Contains(p.Name))
            .ToList();

        await _roleManager.SetGrantedPermissionsAsync(role, grantedPermissions);
    }
}
```

```csharp
user permissions

public class UserAppService : IUserAppService
{
    private readonly UserManager _userManager;
    private readonly IPermissionManager _permissionManager;

    public UserAppService(UserManager userManager, IPermissionManager permissionManager)
    {
        _userManager = userManager;
        _permissionManager = permissionManager;
    }

    public async Task ProhibitPermission(ProhibitPermissionInput input)
    {
        var user = await _userManager.GetUserByIdAsync(input.UserId);
        var permission = _permissionManager.GetPermission(input.PermissionName);

        await _userManager.ProhibitPermissionAsync(user, permission);
    }
}
```

### authorization

defining permissions

```csharp
public class MyAuthorizationProvider : AuthorizationProvider
{
    public override void SetPermissions(IPermissionDefinitionContext context)
    {
        var administration = context.CreatePermission("Administration");

        var userManagement = administration.CreateChildPermission("Administration.UserManagement");
        userManagement.CreateChildPermission("Administration.UserManagement.CreateUser");

        var roleManagement = administration.CreateChildPermission("Administration.RoleManagement");
    }
}
```

use permission

```csharp
[AbpAuthorize("Administration.UserManagement.CreateUser")]
public void CreateUser(CreateUserInput input)
{
    //A user can not execute this method if he is not granted the "Administration.UserManagement.CreateUser" permission.
}
```

## emacs eglot

どういう単位でgroupingされるんだろう？

```
(insert (cdr (project-current)))
;; ~/vboxshare/venvs/my/individual-sandbox/

(eglot--guess-contact nil)
(markdown-mode (vc . "~/vboxshare/venvs/my/individual-sandbox/") eglot-lsp-server ("efm-langserver" "-logfile" "/tmp/efm.log" "-loglevel" "10"))
```

eglotはproject-currentごとか。。


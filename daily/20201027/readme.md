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


import typing_extensions as tx
import typing as t
import objects


@first_tag("User and Authentication")
class User_and_AuthenticationService:
    @endpoint(path="/users/login", method="POST", summary="Existing user login")
    def Login(
        self,
        body: tx.Annotated[objects.LoginUserRequest, Description("Credentials to use")],
    ):
        """
        Login for existing user
        """
        pass

    @endpoint(path="/users", method="POST", summary="Register a new user")
    def CreateUser(
        self,
        body: tx.Annotated[
            objects.NewUserRequest, Description("Details of the new user to register")
        ],
    ):
        """
        Register a new user
        """
        pass

    @endpoint(path="/users", method="GET", summary="Get current user")
    def GetCurrentUser(self):
        """
        Gets the currently logged-in user
        """
        pass

    @endpoint(path="/users", method="PUT", summary="Update current user")
    def UpdateCurrentUser(
        self,
        body: tx.Annotated[
            objects.UpdateUserRequest,
            Description("User details to update. At least **one** field is required."),
        ],
    ):
        """
        Updated user information for current user
        """
        pass


@first_tag("Profile")
class ProfileService:
    @endpoint(path="/profiles/{username}", method="GET", summary="Get a profile")
    def GetProfileByUsername(self):
        """
        Get a profile of a user of the system. Auth is optional
        """
        pass

    @endpoint(
        path="/profiles/{username}/follow", method="POST", summary="Follow a user"
    )
    def FollowUserByUsername(self):
        """
        Follow a user by username
        """
        pass

    @endpoint(
        path="/profiles/{username}/follow", method="DELETE", summary="Unfollow a user"
    )
    def UnfollowUserByUsername(self):
        """
        Unfollow a user by username
        """
        pass


@first_tag("Articles")
class ArticlesService:
    @endpoint(
        path="/articles/feed",
        method="GET",
        summary="Get recent articles from users you follow",
    )
    def GetArticlesFeed(
        self,
        limit: tx.Annotated[
            t.Optional[Query[integer]],
            Description("Limit number of articles returned (default is 20)"),
        ],
        offset: tx.Annotated[
            t.Optional[Query[integer]],
            Description("Offset/skip number of articles (default is 0)"),
        ],
    ):
        """
        Get most recent articles from users you follow. Use query parameters to limit. Auth is required
        """
        pass

    @endpoint(path="/articles", method="GET", summary="Get recent articles globally")
    def GetArticles(
        self,
        tag: tx.Annotated[t.Optional[Query[string]], Description("Filter by tag")],
        author: tx.Annotated[
            t.Optional[Query[string]], Description("Filter by author (username)")
        ],
        favorited: tx.Annotated[
            t.Optional[Query[string]],
            Description("Filter by favorites of a user (username)"),
        ],
        limit: tx.Annotated[
            t.Optional[Query[integer]],
            Description("Limit number of articles returned (default is 20)"),
        ],
        offset: tx.Annotated[
            t.Optional[Query[integer]],
            Description("Offset/skip number of articles (default is 0)"),
        ],
    ):
        """
        Get most recent articles globally. Use query parameters to filter results. Auth is optional
        """
        pass

    @endpoint(path="/articles", method="POST", summary="Create an article")
    def CreateArticle(
        self,
        article: tx.Annotated[
            objects.NewArticleRequest, Description("Article to create")
        ],
    ):
        """
        Create an article. Auth is required
        """
        pass

    @endpoint(path="/articles/{slug}", method="GET", summary="Get an article")
    def GetArticle(self):
        """
        Get an article. Auth not required
        """
        pass

    @endpoint(path="/articles/{slug}", method="PUT", summary="Update an article")
    def UpdateArticle(
        self,
        article: tx.Annotated[
            objects.UpdateArticleRequest, Description("Article to update")
        ],
    ):
        """
        Update an article. Auth is required
        """
        pass

    @endpoint(path="/articles/{slug}", method="DELETE", summary="Delete an article")
    def DeleteArticle(self):
        """
        Delete an article. Auth is required
        """
        pass


@first_tag("Comments")
class CommentsService:
    @endpoint(
        path="/articles/{slug}/comments",
        method="GET",
        summary="Get comments for an article",
    )
    def GetArticleComments(self):
        """
        Get the comments for an article. Auth is optional
        """
        pass

    @endpoint(
        path="/articles/{slug}/comments",
        method="POST",
        summary="Create a comment for an article",
    )
    def CreateArticleComment(
        self,
        comment: tx.Annotated[
            objects.NewCommentRequest, Description("Comment you want to create")
        ],
    ):
        """
        Create a comment for an article. Auth is required
        """
        pass

    @endpoint(
        path="/articles/{slug}/comments/{id}",
        method="DELETE",
        summary="Delete a comment for an article",
    )
    def DeleteArticleComment(self):
        """
        Delete a comment for an article. Auth is required
        """
        pass


@first_tag("Favorites")
class FavoritesService:
    @endpoint(
        path="/articles/{slug}/favorite", method="POST", summary="Favorite an article"
    )
    def CreateArticleFavorite(self):
        """
        Favorite an article. Auth is required
        """
        pass

    @endpoint(
        path="/articles/{slug}/favorite",
        method="DELETE",
        summary="Unfavorite an article",
    )
    def DeleteArticleFavorite(self):
        """
        Unfavorite an article. Auth is required
        """
        pass


@first_tag("Unknown")
class UnknownService:
    @endpoint(path="/tags", method="GET", summary="Get tags")
    def unknown___tags__get(self):
        """
        Get tags. Auth not required
        """
        pass

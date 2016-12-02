package autogen

import (
	"github.com/go-openapi/strfmt"
	"time"
)

/* structure
Github
    Organization
    Owner
    Parent
        Owner
        Permissions
    Permissions
    Source
*/
type Github struct {
	CloneURL         strfmt.Uri   `json:"clone_url" example:"https://github.com/octocat/Hello-World.git"`
	CreatedAt        time.Time    `json:"created_at" example:"2011-01-26T19:01:12Z"`
	DefaultBranch    string       `json:"default_branch" example:"master"`
	Description      string       `json:"description" example:"This your first repo!"`
	Fork             int          `json:"fork" example:"False"`
	ForksCount       int          `json:"forks_count" example:"9"`
	FullName         string       `json:"full_name" example:"octocat/Hello-World"`
	GitURL           strfmt.Uri   `json:"git_url" example:"git://github.com/octocat/Hello-World.git"`
	HTMLURL          strfmt.Uri   `json:"html_url" example:"https://github.com/octocat/Hello-World"`
	HasDownloads     int          `json:"has_downloads" example:"True"`
	HasIssues        int          `json:"has_issues" example:"True"`
	HasWiki          int          `json:"has_wiki" example:"True"`
	Homepage         strfmt.Uri   `json:"homepage" example:"https://github.com"`
	ID               int          `json:"id" example:"1296269"`
	Language         interface{}  `json:"language" example:"None"`
	MirrorURL        strfmt.Uri   `json:"mirror_url" example:"git://git.example.com/octocat/Hello-World"`
	Name             string       `json:"name" example:"Hello-World"`
	OpenIssuesCount  int          `json:"open_issues_count" example:"0"`
	Organization     Organization `json:"organization"`
	Owner            Organization `json:"owner"`
	Parent           Parent       `json:"parent"`
	Permissions      Permissions  `json:"permissions"`
	Private          int          `json:"private" example:"False"`
	PushedAt         time.Time    `json:"pushed_at" example:"2011-01-26T19:06:43Z"`
	SSHURL           string       `json:"ssh_url" example:"git@github.com:octocat/Hello-World.git"`
	Size             int          `json:"size" example:"108"`
	Source           Parent       `json:"source"`
	StargazersCount  int          `json:"stargazers_count" example:"80"`
	SubscribersCount int          `json:"subscribers_count" example:"42"`
	SvnURL           strfmt.Uri   `json:"svn_url" example:"https://svn.github.com/octocat/Hello-World"`
	URL              strfmt.Uri   `json:"url" example:"https://api.github.com/repos/octocat/Hello-World"`
	UpdatedAt        time.Time    `json:"updated_at" example:"2011-01-26T19:14:43Z"`
	WatchersCount    int          `json:"watchers_count" example:"80"`
}

type Organization struct {
	AvatarURL         strfmt.Uri `json:"avatar_url" example:"https://github.com/images/error/octocat_happy.gif"`
	EventsURL         strfmt.Uri `json:"events_url" example:"https://api.github.com/users/octocat/events{/privacy}"`
	FollowersURL      strfmt.Uri `json:"followers_url" example:"https://api.github.com/users/octocat/followers"`
	FollowingURL      strfmt.Uri `json:"following_url" example:"https://api.github.com/users/octocat/following{/other_user}"`
	GistsURL          strfmt.Uri `json:"gists_url" example:"https://api.github.com/users/octocat/gists{/gist_id}"`
	GravatarID        string     `json:"gravatar_id" example:"somehexcode"`
	HTMLURL           strfmt.Uri `json:"html_url" example:"https://github.com/octocat"`
	ID                int        `json:"id" example:"1"`
	Login             string     `json:"login" example:"octocat"`
	OrganizationsURL  strfmt.Uri `json:"organizations_url" example:"https://api.github.com/users/octocat/orgs"`
	ReceivedEventsURL strfmt.Uri `json:"received_events_url" example:"https://api.github.com/users/octocat/received_events"`
	ReposURL          strfmt.Uri `json:"repos_url" example:"https://api.github.com/users/octocat/repos"`
	SiteAdmin         int        `json:"site_admin" example:"False"`
	StarredURL        strfmt.Uri `json:"starred_url" example:"https://api.github.com/users/octocat/starred{/owner}{/repo}"`
	SubscriptionsURL  strfmt.Uri `json:"subscriptions_url" example:"https://api.github.com/users/octocat/subscriptions"`
	Type              string     `json:"type" example:"Organization"`
	URL               strfmt.Uri `json:"url" example:"https://api.github.com/users/octocat"`
}

type Parent struct {
	CloneURL        strfmt.Uri   `json:"clone_url" example:"https://github.com/octocat/Hello-World.git"`
	CreatedAt       time.Time    `json:"created_at" example:"2011-01-26T19:01:12Z"`
	DefaultBranch   string       `json:"default_branch" example:"master"`
	Description     string       `json:"description" example:"This your first repo!"`
	Fork            int          `json:"fork" example:"True"`
	ForksCount      int          `json:"forks_count" example:"9"`
	FullName        string       `json:"full_name" example:"octocat/Hello-World"`
	GitURL          strfmt.Uri   `json:"git_url" example:"git://github.com/octocat/Hello-World.git"`
	HTMLURL         strfmt.Uri   `json:"html_url" example:"https://github.com/octocat/Hello-World"`
	HasDownloads    int          `json:"has_downloads" example:"True"`
	HasIssues       int          `json:"has_issues" example:"True"`
	HasWiki         int          `json:"has_wiki" example:"True"`
	Homepage        strfmt.Uri   `json:"homepage" example:"https://github.com"`
	ID              int          `json:"id" example:"1296269"`
	Language        interface{}  `json:"language" example:"None"`
	MirrorURL       strfmt.Uri   `json:"mirror_url" example:"git://git.example.com/octocat/Hello-World"`
	Name            string       `json:"name" example:"Hello-World"`
	OpenIssuesCount int          `json:"open_issues_count" example:"0"`
	Owner           Organization `json:"owner"`
	Permissions     Permissions  `json:"permissions"`
	Private         int          `json:"private" example:"False"`
	PushedAt        time.Time    `json:"pushed_at" example:"2011-01-26T19:06:43Z"`
	SSHURL          string       `json:"ssh_url" example:"git@github.com:octocat/Hello-World.git"`
	Size            int          `json:"size" example:"108"`
	StargazersCount int          `json:"stargazers_count" example:"80"`
	SvnURL          strfmt.Uri   `json:"svn_url" example:"https://svn.github.com/octocat/Hello-World"`
	URL             strfmt.Uri   `json:"url" example:"https://api.github.com/repos/octocat/Hello-World"`
	UpdatedAt       time.Time    `json:"updated_at" example:"2011-01-26T19:14:43Z"`
	WatchersCount   int          `json:"watchers_count" example:"80"`
}

type Permissions struct {
	Admin int `json:"admin" example:"False"`
	Pull  int `json:"pull" example:"True"`
	Push  int `json:"push" example:"False"`
}

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
	CloneURL         strfmt.Uri   `json:"clone_url"`
	CreatedAt        time.Time    `json:"created_at"`
	DefaultBranch    string       `json:"default_branch"`
	Description      string       `json:"description"`
	Fork             int          `json:"fork"`
	ForksCount       int          `json:"forks_count"`
	FullName         string       `json:"full_name"`
	GitURL           strfmt.Uri   `json:"git_url"`
	HTMLURL          strfmt.Uri   `json:"html_url"`
	HasDownloads     int          `json:"has_downloads"`
	HasIssues        int          `json:"has_issues"`
	HasWiki          int          `json:"has_wiki"`
	Homepage         strfmt.Uri   `json:"homepage"`
	ID               int          `json:"id"`
	Language         interface{}  `json:"language"`
	MirrorURL        strfmt.Uri   `json:"mirror_url"`
	Name             string       `json:"name"`
	OpenIssuesCount  int          `json:"open_issues_count"`
	Organization     Organization `json:"organization"`
	Owner            Organization `json:"owner"`
	Parent           Parent       `json:"parent"`
	Permissions      Permissions  `json:"permissions"`
	Private          int          `json:"private"`
	PushedAt         time.Time    `json:"pushed_at"`
	SSHURL           string       `json:"ssh_url"`
	Size             int          `json:"size"`
	Source           Parent       `json:"source"`
	StargazersCount  int          `json:"stargazers_count"`
	SubscribersCount int          `json:"subscribers_count"`
	SvnURL           strfmt.Uri   `json:"svn_url"`
	URL              strfmt.Uri   `json:"url"`
	UpdatedAt        time.Time    `json:"updated_at"`
	WatchersCount    int          `json:"watchers_count"`
}

type Organization struct {
	AvatarURL         strfmt.Uri `json:"avatar_url"`
	EventsURL         strfmt.Uri `json:"events_url"`
	FollowersURL      strfmt.Uri `json:"followers_url"`
	FollowingURL      strfmt.Uri `json:"following_url"`
	GistsURL          strfmt.Uri `json:"gists_url"`
	GravatarID        string     `json:"gravatar_id"`
	HTMLURL           strfmt.Uri `json:"html_url"`
	ID                int        `json:"id"`
	Login             string     `json:"login"`
	OrganizationsURL  strfmt.Uri `json:"organizations_url"`
	ReceivedEventsURL strfmt.Uri `json:"received_events_url"`
	ReposURL          strfmt.Uri `json:"repos_url"`
	SiteAdmin         int        `json:"site_admin"`
	StarredURL        strfmt.Uri `json:"starred_url"`
	SubscriptionsURL  strfmt.Uri `json:"subscriptions_url"`
	Type              string     `json:"type"`
	URL               strfmt.Uri `json:"url"`
}

type Parent struct {
	CloneURL        strfmt.Uri   `json:"clone_url"`
	CreatedAt       time.Time    `json:"created_at"`
	DefaultBranch   string       `json:"default_branch"`
	Description     string       `json:"description"`
	Fork            int          `json:"fork"`
	ForksCount      int          `json:"forks_count"`
	FullName        string       `json:"full_name"`
	GitURL          strfmt.Uri   `json:"git_url"`
	HTMLURL         strfmt.Uri   `json:"html_url"`
	HasDownloads    int          `json:"has_downloads"`
	HasIssues       int          `json:"has_issues"`
	HasWiki         int          `json:"has_wiki"`
	Homepage        strfmt.Uri   `json:"homepage"`
	ID              int          `json:"id"`
	Language        interface{}  `json:"language"`
	MirrorURL       strfmt.Uri   `json:"mirror_url"`
	Name            string       `json:"name"`
	OpenIssuesCount int          `json:"open_issues_count"`
	Owner           Organization `json:"owner"`
	Permissions     Permissions  `json:"permissions"`
	Private         int          `json:"private"`
	PushedAt        time.Time    `json:"pushed_at"`
	SSHURL          string       `json:"ssh_url"`
	Size            int          `json:"size"`
	StargazersCount int          `json:"stargazers_count"`
	SvnURL          strfmt.Uri   `json:"svn_url"`
	URL             strfmt.Uri   `json:"url"`
	UpdatedAt       time.Time    `json:"updated_at"`
	WatchersCount   int          `json:"watchers_count"`
}

type Permissions struct {
	Admin int `json:"admin"`
	Pull  int `json:"pull"`
	Push  int `json:"push"`
}

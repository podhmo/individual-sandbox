package autogen

import (
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
	CloneURL         string       `json:"clone_url"`
	CreatedAt        time.Time    `json:"created_at"`
	DefaultBranch    string       `json:"default_branch"`
	Description      string       `json:"description"`
	Fork             int          `json:"fork"`
	ForksCount       int          `json:"forks_count"`
	FullName         string       `json:"full_name"`
	GitURL           string       `json:"git_url"`
	HTMLURL          string       `json:"html_url"`
	HasDownloads     int          `json:"has_downloads"`
	HasIssues        int          `json:"has_issues"`
	HasWiki          int          `json:"has_wiki"`
	Homepage         string       `json:"homepage"`
	ID               int          `json:"id"`
	Language         interface{}  `json:"language"`
	MirrorURL        string       `json:"mirror_url"`
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
	SvnURL           string       `json:"svn_url"`
	URL              string       `json:"url"`
	UpdatedAt        time.Time    `json:"updated_at"`
	WatchersCount    int          `json:"watchers_count"`
}

type Organization struct {
	AvatarURL         string `json:"avatar_url"`
	EventsURL         string `json:"events_url"`
	FollowersURL      string `json:"followers_url"`
	FollowingURL      string `json:"following_url"`
	GistsURL          string `json:"gists_url"`
	GravatarID        string `json:"gravatar_id"`
	HTMLURL           string `json:"html_url"`
	ID                int    `json:"id"`
	Login             string `json:"login"`
	OrganizationsURL  string `json:"organizations_url"`
	ReceivedEventsURL string `json:"received_events_url"`
	ReposURL          string `json:"repos_url"`
	SiteAdmin         int    `json:"site_admin"`
	StarredURL        string `json:"starred_url"`
	SubscriptionsURL  string `json:"subscriptions_url"`
	Type              string `json:"type"`
	URL               string `json:"url"`
}

type Parent struct {
	CloneURL        string       `json:"clone_url"`
	CreatedAt       time.Time    `json:"created_at"`
	DefaultBranch   string       `json:"default_branch"`
	Description     string       `json:"description"`
	Fork            int          `json:"fork"`
	ForksCount      int          `json:"forks_count"`
	FullName        string       `json:"full_name"`
	GitURL          string       `json:"git_url"`
	HTMLURL         string       `json:"html_url"`
	HasDownloads    int          `json:"has_downloads"`
	HasIssues       int          `json:"has_issues"`
	HasWiki         int          `json:"has_wiki"`
	Homepage        string       `json:"homepage"`
	ID              int          `json:"id"`
	Language        interface{}  `json:"language"`
	MirrorURL       string       `json:"mirror_url"`
	Name            string       `json:"name"`
	OpenIssuesCount int          `json:"open_issues_count"`
	Owner           Organization `json:"owner"`
	Permissions     Permissions  `json:"permissions"`
	Private         int          `json:"private"`
	PushedAt        time.Time    `json:"pushed_at"`
	SSHURL          string       `json:"ssh_url"`
	Size            int          `json:"size"`
	StargazersCount int          `json:"stargazers_count"`
	SvnURL          string       `json:"svn_url"`
	URL             string       `json:"url"`
	UpdatedAt       time.Time    `json:"updated_at"`
	WatchersCount   int          `json:"watchers_count"`
}

type Permissions struct {
	Admin int `json:"admin"`
	Pull  int `json:"pull"`
	Push  int `json:"push"`
}

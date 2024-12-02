// 標準ライブラリだけでHTTPリクエストを行うコード例
async function postToBluesky(
    username: string,
    password: string,
    content: string,
) {
    const baseUrl = "https://bsky.social/xrpc";

    // 認証エンドポイント
    const authEndpoint = `${baseUrl}/com.atproto.server.createSession`;

    // 認証リクエスト
    const authResponse = await fetch(authEndpoint, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify({
            identifier: username,
            password: password,
        }),
    });

    if (!authResponse.ok) {
        throw new Error(`Failed to authenticate: ${authResponse.statusText}`);
    }

    const authData = await authResponse.json();
    const accessJwt = authData.accessJwt;

    // 投稿エンドポイント
    const postEndpoint = `${baseUrl}/com.atproto.repo.createRecord`;

    // 現在時刻をISO 8601形式で取得
    const createdAt = new Date().toISOString();

    // 投稿データ
    const postData = {
        repo: authData.did, // ユーザーのDID
        collection: "app.bsky.feed.post",
        record: {
            $type: "app.bsky.feed.post",
            text: content,
            createdAt,
        },
    };

    // 投稿リクエスト
    const postResponse = await fetch(postEndpoint, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            Authorization: `Bearer ${accessJwt}`,
        },
        body: JSON.stringify(postData),
    });

    if (!postResponse.ok) {
        throw new Error(`Failed to post: ${postResponse.statusText}`);
    }

    const postResult = await postResponse.json();
    console.log("Post successful:", postResult);
}

// 使用例
(async () => {
    const username = "your-username"; // Blueskyのユーザー名
    const password = "your-password"; // パスワード
    const content = "Hello, Bluesky! This is a test post.";

    try {
        await postToBluesky(username, password, content);
    } catch (error) {
        console.error(error);
    }
})();

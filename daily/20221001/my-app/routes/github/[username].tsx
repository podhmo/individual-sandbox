import { Handlers, PageProps } from "$fresh/server.ts";

interface User {
    login: string;
    name: string;
    avatar_url: string;
}

export const handler: Handlers<User | null>  = {
    async GET(_, ctx) {
        const { username } = ctx.params;
        const resp = await fetch(`https://api.github.com/users/${username}`);
        if (resp.status == 404) {
            // return ctx.renderNotFound();
            return ctx.render(null)
        }
        const user: User = await resp.json();
        return ctx.render(user);
    }
}

export default function Page({ data }: PageProps<User | null>) {
    if (!data) {
      return <h1>User not found</h1>;
    }
  
    return (
      <div>
        <img src={data.avatar_url} width={64} height={64} />
        <h1>{data.name}</h1>
        <p>{data.login}</p>
      </div>
    );
}

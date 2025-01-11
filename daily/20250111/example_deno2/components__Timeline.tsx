/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import { useState } from 'npm:react@19'
import TweetInput from './components__TweetInput.tsx'
import Tweet from './components__Tweet.tsx'

interface TweetData {
  id: number
  author: string
  handle: string
  content: string
  timestamp: string
  likes: number
  retweets: number
  avatar: string
}

const initialTweets: TweetData[] = [
  {
    id: 1,
    author: "John Doe",
    handle: "@johndoe",
    content: "Just had an amazing coffee at the new café downtown! ☕️ #CoffeeLovers",
    timestamp: "2h",
    likes: 15,
    retweets: 5,
    avatar: "/placeholder.svg?height=40&width=40"
  },
  {
    id: 2,
    author: "Jane Smith",
    handle: "@janesmith",
    content: "Excited to announce my new book 'React Mastery' is now available for pre-order! 📚 #ReactJS #WebDev",
    timestamp: "4h",
    likes: 32,
    retweets: 12,
    avatar: "/placeholder.svg?height=40&width=40"
  },
  {
    id: 3,
    author: "Tech News",
    handle: "@technews",
    content: "Breaking: New AI model can generate realistic 3D scenes from text descriptions. 🤖🎨 #AI #MachineLearning",
    timestamp: "6h",
    likes: 78,
    retweets: 45,
    avatar: "/placeholder.svg?height=40&width=40"
  }
]

export default function Timeline() {
  const [tweets, setTweets] = useState<TweetData[]>(initialTweets)

  const handleNewTweet = (content: string) => {
    const newTweet: TweetData = {
      id: tweets.length + 1,
      author: "Current User",
      handle: "@currentuser",
      content,
      timestamp: "Just now",
      likes: 0,
      retweets: 0,
      avatar: "/placeholder.svg?height=40&width=40"
    }
    setTweets([newTweet, ...tweets])
  }

  return (
    <div className="max-w-2xl mx-auto">
      <TweetInput onNewTweet={handleNewTweet} />
      <div className="divide-y divide-gray-200">
        {tweets.map((tweet) => (
          <Tweet key={tweet.id} tweet={tweet} />
        ))}
      </div>
    </div>
  )
}


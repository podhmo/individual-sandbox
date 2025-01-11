/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import { useState } from 'npm:react@19'
import { Avatar, AvatarFallback, AvatarImage } from './components__ui__avatar.tsx'
import { Heart, MessageCircle, Repeat2 } from 'npm:lucide-react'

interface TweetProps {
  tweet: {
    author: string
    handle: string
    content: string
    timestamp: string
    likes: number
    retweets: number
    avatar: string
  }
}

export default function Tweet({ tweet }: TweetProps) {
  const [likes, setLikes] = useState(tweet.likes)
  const [retweets, setRetweets] = useState(tweet.retweets)

  const handleLike = () => setLikes(likes + 1)
  const handleRetweet = () => setRetweets(retweets + 1)

  return (
    <div className="flex space-x-3 p-4">
      <Avatar>
        <AvatarImage src={tweet.avatar} alt={tweet.author} />
        <AvatarFallback>{tweet.author[0]}</AvatarFallback>
      </Avatar>
      <div className="flex-1">
        <div className="flex items-center space-x-1">
          <span className="font-bold">{tweet.author}</span>
          <span className="text-gray-500">{tweet.handle}</span>
          <span className="text-gray-500">·</span>
          <span className="text-gray-500">{tweet.timestamp}</span>
        </div>
        <p className="mt-1">{tweet.content}</p>
        <div className="flex justify-between mt-3 text-gray-500">
          <button className="flex items-center space-x-2 hover:text-blue-500">
            <MessageCircle className="w-5 h-5" />
            <span>Reply</span>
          </button>
          <button className="flex items-center space-x-2 hover:text-green-500" onClick={handleRetweet}>
            <Repeat2 className="w-5 h-5" />
            <span>{retweets}</span>
          </button>
          <button className="flex items-center space-x-2 hover:text-red-500" onClick={handleLike}>
            <Heart className="w-5 h-5" />
            <span>{likes}</span>
          </button>
        </div>
      </div>
    </div>
  )
}


/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import { useState } from 'npm:react@19'
import { Button } from './components__ui__button.tsx'
import { Textarea } from './components__ui__textarea.tsx'
import { Avatar, AvatarFallback, AvatarImage } from './components__ui__avatar.tsx'

interface TweetInputProps {
  onNewTweet: (content: string) => void
}

export default function TweetInput({ onNewTweet }: TweetInputProps) {
  const [content, setContent] = useState('')

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    if (content.trim()) {
      onNewTweet(content)
      setContent('')
    }
  }

  return (
    <form onSubmit={handleSubmit} className="flex items-start space-x-4 p-4 border-b border-gray-200">
      <Avatar>
        <AvatarImage src="/placeholder.svg?height=40&width=40" alt="User" />
        <AvatarFallback>U</AvatarFallback>
      </Avatar>
      <div className="flex-1">
        <Textarea
          value={content}
          onChange={(e) => setContent(e.target.value)}
          placeholder="What's happening?"
          className="w-full mb-2"
          rows={3}
        />
        <Button type="submit" disabled={!content.trim()}>
          Tweet
        </Button>
      </div>
    </form>
  )
}


/**
 * Estimates reading time for case study pages.
 * Uses a word count / 200 WPM formula.
 */
export default function ReadingTime({ wordCount }: { wordCount: number }) {
  const minutes = Math.max(1, Math.ceil(wordCount / 200))
  return (
    <span className="font-mono text-xs text-titanium/50">
      {minutes} min read
    </span>
  )
}

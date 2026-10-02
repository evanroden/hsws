interface TiltCardProps {
  children: React.ReactNode
  className?: string
  /** Kept for API compatibility; the card no longer tilts. */
  maxTilt?: number
  glare?: number
}

/**
 * Card wrapper with a subtle lift on hover.
 * The earlier 3D tilt rotated the card under the pointer, which could move the link out
 * from under the cursor and drop its hover/click state in some browsers, so it was removed.
 */
export default function TiltCard({ children, className = '' }: TiltCardProps) {
  return (
    <div
      className={`relative h-full transition-transform duration-200 ease-out hover:-translate-y-1 motion-reduce:transition-none motion-reduce:hover:translate-y-0 ${className}`}
    >
      {children}
    </div>
  )
}

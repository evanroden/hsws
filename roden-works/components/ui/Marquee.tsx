'use client'

import { motion } from 'framer-motion'

interface MarqueeProps {
  children: React.ReactNode
  /** Speed in seconds for one full cycle */
  speed?: number
  /** Pause on hover */
  pauseOnHover?: boolean
  /** Direction */
  direction?: 'left' | 'right'
  className?: string
}

/**
 * Infinite horizontal marquee/ticker.
 * Duplicates children for seamless looping.
 */
export default function Marquee({
  children,
  speed = 30,
  pauseOnHover = true,
  direction = 'left',
  className = '',
}: MarqueeProps) {
  return (
    <div
      className={`overflow-hidden relative ${className}`}
      style={{ maskImage: 'linear-gradient(to right, transparent, black 10%, black 90%, transparent)' }}
    >
      <motion.div
        className={`flex gap-8 w-max ${pauseOnHover ? 'hover:[animation-play-state:paused]' : ''}`}
        animate={{
          x: direction === 'left' ? ['0%', '-50%'] : ['-50%', '0%'],
        }}
        transition={{
          x: {
            duration: speed,
            repeat: Infinity,
            ease: 'linear',
          },
        }}
      >
        {children}
        {children}
      </motion.div>
    </div>
  )
}

/* ─── Divider Marquee with discipline keywords ───── */

export function DisciplineMarquee() {
  const items = [
    'Biomedical Engineering',
    'Energy-as-a-Service',
    'Organ Donation Policy',
    'Climate Advocacy',
    'Cinematography',
    'Urban Planning',
    'Prosthetic Design',
    'ERP Architecture',
    'Public Health',
    'Glass Art',
    'TEDx Speaker',
    'Sustainability',
  ]

  return (
    <Marquee speed={40} className="py-8 border-y border-white/5">
      {items.map((item) => (
        <span
          key={item}
          className="flex items-center gap-8 font-mono text-sm tracking-widest uppercase text-titanium/30 whitespace-nowrap select-none"
        >
          <span>{item}</span>
          <span className="w-1.5 h-1.5 rounded-full bg-copper/30 flex-shrink-0" />
        </span>
      ))}
    </Marquee>
  )
}

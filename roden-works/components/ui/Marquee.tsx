'use client'

import { useState } from 'react'
import Link from 'next/link'
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
  const [paused, setPaused] = useState(false)

  return (
    <div
      className={`overflow-hidden relative ${className}`}
      style={{ maskImage: 'linear-gradient(to right, transparent, black 10%, black 90%, transparent)' }}
      onMouseEnter={pauseOnHover ? () => setPaused(true) : undefined}
      onMouseLeave={pauseOnHover ? () => setPaused(false) : undefined}
      onFocus={() => setPaused(true)}
      onBlur={() => setPaused(false)}
    >
      <motion.div
        className="flex gap-8 w-max"
        animate={paused ? undefined : {
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

const disciplineLinks: { label: string; href: string }[] = [
  { label: 'Biomedical Engineering', href: '/engineering-and-sustainability/research/va-prosthetics' },
  { label: 'Energy-as-a-Service', href: '/engineering-and-sustainability/enfra' },
  { label: 'Organ Donation Policy', href: '/advocacy-and-civic/ycod' },
  { label: 'Climate Advocacy', href: '/advocacy-and-civic/our-climate' },
  { label: 'Cinematography', href: '/studio/cinematography' },
  { label: 'Urban Planning', href: '/advocacy-and-civic/midtown-metairie' },
  { label: 'Prosthetic Design', href: '/engineering-and-sustainability/research/va-prosthetics' },
  { label: 'ERP Architecture', href: '/engineering-and-sustainability/odoo' },
  { label: 'Public Health', href: '/engineering-and-sustainability/research/haps' },
  { label: 'Glass Art', href: '/studio/glass-art' },
  { label: 'TEDx Speaker', href: '/about/ted' },
  { label: 'Sustainability', href: '/engineering-and-sustainability' },
]

export function DisciplineMarquee() {
  return (
    <Marquee speed={40} className="py-8 border-y border-white/5">
      {disciplineLinks.map((item) => (
        <span
          key={item.label}
          className="flex items-center gap-8 font-mono text-sm tracking-widest uppercase whitespace-nowrap select-none"
        >
          <Link
            href={item.href}
            className="text-titanium/30 hover:text-copper transition-colors duration-300"
            tabIndex={0}
          >
            {item.label}
          </Link>
          <span className="w-1.5 h-1.5 rounded-full bg-copper/30 flex-shrink-0" />
        </span>
      ))}
    </Marquee>
  )
}

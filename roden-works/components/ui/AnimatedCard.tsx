'use client'

import { motion } from 'framer-motion'
import Link from 'next/link'
import { useInView } from '@/lib/hooks'

interface AnimatedCardProps {
  href: string
  title: string
  description: string
  label?: string
  index?: number
  children?: React.ReactNode
}

export default function AnimatedCard({
  href,
  title,
  description,
  label,
  index = 0,
  children,
}: AnimatedCardProps) {
  const { ref, isInView } = useInView(0.1)

  return (
    <motion.div
      ref={ref}
      initial={{ opacity: 0, y: 30 }}
      animate={isInView ? { opacity: 1, y: 0 } : {}}
      transition={{ duration: 0.6, delay: index * 0.1, ease: [0.16, 1, 0.3, 1] }}
    >
      <Link href={href} className="group block">
        <div className="glass rounded-xl p-6 md:p-8 h-full transition-all duration-500 group-hover:bg-white/10 group-hover:border-white/20 group-hover:scale-[1.02] group-hover:shadow-2xl group-hover:shadow-forest/10">
          {label && (
            <span className="inline-block font-mono text-xs tracking-widest uppercase text-copper mb-3">
              {label}
            </span>
          )}
          {children}
          <h3 className="font-serif text-xl md:text-2xl text-white mb-3 group-hover:text-copper transition-colors">
            {title}
          </h3>
          <p className="text-titanium text-sm leading-relaxed">{description}</p>
          <div className="mt-6 flex items-center gap-2 text-sm text-titanium group-hover:text-white transition-colors">
            <span>Explore</span>
            <motion.span
              className="inline-block"
              animate={{ x: [0, 4, 0] }}
              transition={{ repeat: Infinity, duration: 1.5, ease: 'easeInOut' }}
            >
              →
            </motion.span>
          </div>
        </div>
      </Link>
    </motion.div>
  )
}

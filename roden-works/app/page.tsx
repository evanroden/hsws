import Hero from '@/components/home/Hero'
import BentoGrid from '@/components/home/BentoGrid'
import ImpactStats from '@/components/home/ImpactStats'
import FeaturedWork from '@/components/home/FeaturedWork'
import AboutTeaser from '@/components/home/AboutTeaser'
import { DisciplineMarquee } from '@/components/ui/Marquee'

export default function HomePage() {
  return (
    <>
      <Hero />
      <DisciplineMarquee />
      <BentoGrid />
      <ImpactStats />
      <FeaturedWork />
      <AboutTeaser />
    </>
  )
}

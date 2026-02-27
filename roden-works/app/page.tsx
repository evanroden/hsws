import Hero from '@/components/home/Hero'
import BentoGrid from '@/components/home/BentoGrid'
import ImpactStats from '@/components/home/ImpactStats'
import FeaturedWork from '@/components/home/FeaturedWork'
import AboutTeaser from '@/components/home/AboutTeaser'
import PressSection from '@/components/home/PressSection'
import { DisciplineMarquee } from '@/components/ui/Marquee'
import { WebSiteJsonLd } from '@/components/seo/JsonLd'

export default function HomePage() {
  return (
    <>
      <WebSiteJsonLd />
      <Hero />
      <DisciplineMarquee />
      <BentoGrid />
      <ImpactStats />
      <FeaturedWork />
      <PressSection />
      <AboutTeaser />
    </>
  )
}
